{-# OPTIONS_GHC -Wno-partial-fields #-}

module Kore.Trace where

import Control.Monad (when, forM_, forM)
import Control.Monad.Catch (MonadMask, bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON), encode)
import Data.Aeson.Diff (diff)
import Data.Binary (Binary (put, get), Put, Word32, putWord8, Get, get, getWord8)
import Data.Binary.Put (putByteString, runPut)
import Data.Bits
import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack)
import Data.Hashable
import Data.IORef
import Debug.Trace.ByteString (traceEvent, traceEventIO)
import Debug.Trace.Flags
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Natural (Natural)
import Kore.Pattern.Base (Pattern, RewriteResult (..), Term)
import Kore.Syntax.Json (KoreJson, addHeader)
import Kore.Syntax.Json.Externalise (externalisePattern)
import Kore.Pattern.Binary (encodeMagicHeaderAndVersion, Version (Version), encodeTerm, encodePattern, decodePattern, decodeTerm')
import qualified Data.ByteString.Lazy as BL
import Debug.Trace.Binary (traceBinaryEventIO, traceBinaryEvent)
import Data.List.NonEmpty (toList, fromList)
import Data.Binary.Get (getByteString)

data CustomUserEventType = Timing | LlvmCalls | Rewriting deriving (Show, Enum)

enabledCustomUserEventTypes :: IORef Word32
{-# NOINLINE enabledCustomUserEventTypes #-}
enabledCustomUserEventTypes = unsafePerformIO $ newIORef 0

enableCustomUserEvent :: Enum a => a -> IO ()
enableCustomUserEvent a =
    modifyIORef enabledCustomUserEventTypes $ \enabled -> enabled .|. (shiftL 1 $ fromEnum a)

customUserEventEnabled :: Enum a => a -> Bool
customUserEventEnabled a =
    unsafePerformIO $
        readIORef enabledCustomUserEventTypes >>= \enabled ->
            let ba = shiftL 1 $ fromEnum a in pure $ ba == ba .&. enabled

data CustomUserEvent
    = Start ByteString
    | Stop ByteString
    | LlvmCall
        { ret :: Maybe Int
        , call :: ByteString
        , args :: [Either Int ByteString]
        }
    | LlvmVar Term
    | Rewrite (RewriteResult Pattern)

encodeCustomUserEvent :: CustomUserEvent -> Put
encodeCustomUserEvent = \case
    Start ident -> putByteString "START" <> put ident
    Stop ident -> putByteString "STOP " <> put ident
    LlvmCall{ret, call, args} -> putByteString "LLVM " <> put ret <> put call <> put args
    LlvmVar t -> putByteString "LLVMV" <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodeTerm t
    Rewrite res -> putByteString "RWRTE" <> case res of
      RewriteSingle pat           -> putWord8 0 <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat
      RewriteBranch pat pats      -> putWord8 1 <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat <> put (length pats) <> forM_ pats ((encodeMagicHeaderAndVersion (Version 1 1 0) <>) . encodePattern)
      RewriteStuck pat            -> putWord8 2 <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat
      RewriteCutPoint t pat1 pat2 -> putWord8 3 <> put t <> forM_ [pat1, pat2] ((encodeMagicHeaderAndVersion (Version 1 1 0) <>) . encodePattern)
      RewriteTerminal t pat       -> putWord8 4 <> put t <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat
      RewriteStopped pat          -> putWord8 5 <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat
      RewriteAborted pat          -> putWord8 6 <> encodeMagicHeaderAndVersion (Version 1 1 0) <> encodePattern pat

decodeCustomUserEvent :: Get (Maybe CustomUserEvent)
decodeCustomUserEvent = getByteString 5 >>= \case
  "START" -> Just . Start <$> get
  "STOP " -> Just . Stop <$> get
  "LLVM " -> Just <$> (LlvmCall <$> get <*> get <*> get)
  "LLVMV" -> Just . LlvmVar <$> decodeTerm' Nothing
  "RWRTE" -> getWord8 >>= \case
    0 -> Just . Rewrite . RewriteSingle <$> decodePattern Nothing
    1 -> do
      pat <- decodePattern Nothing
      (l :: Int) <- get
      pats <- fromList <$> forM [0..l] (const $ decodePattern Nothing)
      pure $ Just $ Rewrite $ RewriteBranch pat pats
    2 -> Just . Rewrite . RewriteStuck <$> decodePattern Nothing
    3 -> Just . Rewrite <$> (RewriteCutPoint <$> get <*> decodePattern Nothing <*> decodePattern Nothing)
    4 -> Just . Rewrite <$> (RewriteTerminal <$> get <*> decodePattern Nothing)
    5 -> Just . Rewrite . RewriteStopped <$> decodePattern Nothing
    6 -> Just . Rewrite . RewriteAborted <$> decodePattern Nothing
    _ -> error "Invalid RewriteResult"
  _ -> pure Nothing

customUserEventType :: CustomUserEvent -> CustomUserEventType
customUserEventType = \case
    Start _ -> Timing
    Stop _ -> Timing
    LlvmCall{} -> LlvmCalls
    LlvmVar _ -> LlvmCalls
    Rewrite _ -> Rewriting

traceIO :: MonadIO m => CustomUserEvent -> m ()
traceIO e
  | userTracingEnabled && customUserEventEnabled (customUserEventType e) = do
      let message = BL.toStrict $ runPut $ encodeCustomUserEvent e
      when (BS.length message > 2 ^ 16) $ error "eventlog message too long"
      liftIO $ traceBinaryEventIO message
  | otherwise = pure ()

trace :: CustomUserEvent -> a -> a
trace e a
  | userTracingEnabled && customUserEventEnabled (customUserEventType e) = do
      let message = BL.toStrict $ runPut $ encodeCustomUserEvent e
      if (BS.length message > 2 ^ 16)
        then error "eventlog message too long"
        else traceBinaryEvent message a
  | otherwise = a

timeIO :: (MonadIO m, MonadMask m) => ByteString -> m a -> m a
timeIO label
    | userTracingEnabled && customUserEventEnabled Timing =
        bracket_
            (traceIO $ Start label)
            (traceIO $ Stop label)
    | otherwise = id

time :: (Monad m) => ByteString -> m a -> m a
time label m
    | userTracingEnabled && customUserEventEnabled Timing = do
        trace (Start label) $ pure ()
        res <- m
        trace (Stop label) $ pure ()
        pure res
    | otherwise = m

-- data TracingState = TracingState
--     { term :: KoreJson
--     , predicate :: Maybe KoreJson
--     }
--     deriving (Eq, Generic, ToJSON, Hashable)

-- traceState :: MonadIO m => Maybe Pattern -> Pattern -> Natural -> m ()
-- traceState Nothing curr _
--     | userTracingEnabled && eventlogTraceEnabled Rewriting =
--         do
--             let (t, p) = externalisePattern curr
--                 term = TracingState{term = addHeader t, predicate = fmap addHeader p}
--                 hashCurr = pack $ show $ hash term
--                 message = "LLVMC" <> hashCurr <> " " <> toStrict (encode term)
--             when (BS.length message > 2 ^ 16) $ error "LLVM message too long"
--             liftIO $ traceEventIO message
--     | otherwise = pure ()
-- traceState (Just pred) curr counter
--     | userTracingEnabled && eventlogTraceEnabled Rewriting =
--         if counter `mod` 10 == 0
--             then traceState Nothing curr counter
--             else do
--                 let (tCurr, pCurr) = externalisePattern curr
--                     termCurr = TracingState{term = addHeader tCurr, predicate = fmap addHeader pCurr}
--                     (tPred, pPred) = externalisePattern pred
--                     termPred = TracingState{term = addHeader tPred, predicate = fmap addHeader pPred}
--                     hashPred = pack $ show $ hash termPred
--                     hashCurr = pack $ show $ hash termCurr
--                     patch = encode $ diff (toJSON termPred) (toJSON termCurr)
--                     message = "LLVMP" <> hashPred <> " " <> hashCurr <> " " <> toStrict patch
--                 when (BS.length message > 2 ^ 16) $ error "LLVM message too long"
--                 liftIO $ traceEventIO message
--     | otherwise = pure ()
