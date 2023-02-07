module Kore.Trace where

import Control.Monad.Catch (bracket_, MonadMask)
import Debug.Trace.ByteString (traceEventIO, traceEvent)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Char8(pack)
import Data.Binary (Word32)
import GHC.IO (unsafePerformIO)
import Data.Bits
import Debug.Trace.Flags
import Kore.Syntax.Json (KoreJson, addHeader)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), encode)
import Data.Aeson.Diff(diff)
import Kore.Pattern.Base (Pattern)
import Kore.Syntax.Json.Externalise (externalisePattern)
import Data.Hashable
import Data.IORef
import Control.Monad (when)
import GHC.Natural (Natural)


data EventLogTrace = Timing | LlvmCalls | Rewriting deriving (Show, Enum)

eventLogUserEnabledTraces :: IORef Word32
{-# NOINLINE eventLogUserEnabledTraces #-}
eventLogUserEnabledTraces = unsafePerformIO $ newIORef 0


enableEventlogTrace :: Enum a => a -> IO ()
enableEventlogTrace a =
  modifyIORef eventLogUserEnabledTraces $ \enabled -> enabled .|. (shiftL 1 $ fromEnum a)


eventlogTraceEnabled :: Enum a => a -> Bool
eventlogTraceEnabled a =
  unsafePerformIO $ readIORef eventLogUserEnabledTraces >>= \enabled -> let ba = shiftL 1 $ fromEnum a in pure $ ba == ba .&. enabled


event :: (MonadIO m, MonadMask m) => ByteString -> m a -> m a
event label
  | userTracingEnabled && eventlogTraceEnabled Timing = 
        bracket_ (liftIO $ traceEventIO $ "START " <> label)
                 (liftIO $ traceEventIO $ "STOP "  <> label)
  | otherwise = id



eventPure :: (Monad m) => ByteString -> m a -> m a
eventPure label m
  | userTracingEnabled && eventlogTraceEnabled Timing = do
    traceEvent ("START " <> label) $ pure ()
    res <- m
    traceEvent ("STOP " <> label) $ pure ()
    pure res
  | otherwise = m


data TracingState = TracingState {
  term :: KoreJson,
  predicate :: Maybe KoreJson
} deriving (Eq, Generic, ToJSON, Hashable)

traceState :: MonadIO m => Maybe Pattern -> Pattern -> Natural -> m ()
traceState Nothing curr _
  | userTracingEnabled && eventlogTraceEnabled Rewriting = 
    do
      let (t,p) = externalisePattern curr
          term = TracingState{ term = addHeader t, predicate = fmap addHeader p }
          hashCurr = pack $ show $ hash term
          message = "LLVMC" <> hashCurr <> " " <> toStrict (encode term)
      when (BS.length message > 2^16) $ error "LLVM message too long"
      liftIO $ traceEventIO message
  | otherwise = pure ()
traceState (Just pred) curr counter
  | userTracingEnabled && eventlogTraceEnabled Rewriting = if counter `mod` 10 == 0 then traceState Nothing curr counter else
    do
      let (tCurr,pCurr) = externalisePattern curr
          termCurr = TracingState{ term = addHeader tCurr, predicate = fmap addHeader pCurr }
          (tPred,pPred) = externalisePattern pred
          termPred = TracingState{ term = addHeader tPred, predicate = fmap addHeader pPred }
          hashPred = pack $ show $ hash termPred
          hashCurr = pack $ show $ hash termCurr
          patch = encode $ diff (toJSON termPred) (toJSON termCurr)
          message = "LLVMP" <> hashPred <> " " <> hashCurr <> " " <> toStrict patch
      when (BS.length message > 2^16) $ error "LLVM message too long"
      liftIO $ traceEventIO message
  | otherwise = pure ()