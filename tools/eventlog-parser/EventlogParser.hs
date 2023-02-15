module Main (main) where

import Control.Monad (forM_, void)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.List (intersperse, sortOn)
import GHC.IO.Handle (BufferMode (BlockBuffering), Handle, hSetBinaryMode, hSetBuffering)
import GHC.IO.Handle.FD (withBinaryFile)
import GHC.IO.IOMode (IOMode (AppendMode))
import GHC.RTS.Events
import Kore.Trace
import System.Environment (getArgs)

main :: IO ()
main = do
    [eventlogFile] <- getArgs
    eventLogData <- readLog eventlogFile
    withBinaryFile "llvm-calls.c" AppendMode $ \handle -> do
        hSetBinaryMode handle True
        hSetBuffering handle $ BlockBuffering Nothing
        forM_ eventLogData (analyse handle)

type CustomUserEventData = (Timestamp, Either EventInfo CustomUserEvent, Maybe Int)
parseEventLog :: GHC.RTS.Events.Event -> CustomUserEventData
parseEventLog Event{evTime, evSpec, evCap} =
    ( evTime
    , case evSpec of
        UserBinaryMessage msg -> case runGet decodeCustomUserEvent $ BL.fromStrict msg of
            Just userEvent -> Right userEvent
            Nothing -> Left evSpec
        _ -> Left evSpec
    , evCap
    )

readLog :: FilePath -> IO [CustomUserEventData]
readLog file =
    readEventLogFromFile file
        >>= either error (pure . map parseEventLog . sortOn evTime . events . dat)

analyse :: Handle -> CustomUserEventData -> IO ()
analyse llvmCallsFile (evTime, evSpec, evCap) = case evSpec of
    Right LlvmCall{ret, call, args} -> do
        let prettyRet = case ret of
                Just ptr@(BPTerm _) -> lazyByteString "kore_pattern* " <> mkPrettyArg ptr <> lazyByteString " = "
                Just ptr@(BPSort _) -> lazyByteString "kore_sort* " <> mkPrettyArg ptr <> lazyByteString " = "
                Just ptr@(BPSymbol _) -> lazyByteString "kore_symbol* " <> mkPrettyArg ptr <> lazyByteString " = "
                _ -> ""
            mkPrettyArg = \case
                BPTerm h -> hshNm "pat" h
                BPSort h -> hshNm "sort" h
                BPSymbol h -> hshNm "sym" h
                BPString s -> charUtf8 '"' <> byteString s <> charUtf8 '"'
            prettyArgs = charUtf8 '(' <> mconcat (intersperse (charUtf8 ',') $ map mkPrettyArg args) <> charUtf8 ')'
        hPutBuilder llvmCallsFile $ prettyRet <> byteString call <> prettyArgs <> lazyByteString ";\n"
    _ -> pure ()
  where
    hshNm :: BS.ByteString -> Int -> Builder
    hshNm nm h = if h < 0 then byteString nm <> lazyByteString "_m" <> (stringUtf8 $ show $ abs h) else byteString nm <> charUtf8 '_' <> (stringUtf8 $ show h)

-- analyze :: Options -> EventLog -> NonEmpty EventAnalysis
-- analyze log =
--     let AnalysisState _ analyses = execState (mapM_ analyzeEvent (sortedEvents log))
--                                              initialAnalysisState
--     in NonEmpty.reverse $ do
--          analysis <- nonEmptyTail analyses
--          pure analysis { eventTotals = computeTotals (_events analysis)
--                        , eventStarts = computeStarts (_events analysis) }
--   where
--     isWindowEvent :: EventId -> Bool
--     isWindowEvent = case optionsWindowEvent of
--                       Nothing -> const False
--                       Just ev -> (== ev)

--     analyzeEvent :: Event -> State AnalysisState ()
--     analyzeEvent (Event time spec _mb_cap) = do
--       cur $ recordShutdown time
--       case spec of
--         -- CapCreate/CapDelete are the "new" events (ghc >= 7.6)
--         -- Startup/Shutdown are older (to support older eventlogs)
--         CapCreate _cap             -> cur $ recordStartup  time
--         CapDelete _cap             -> cur $ recordShutdown time
--         Startup _numCaps           -> cur $ recordStartup  time
--         Shutdown                   -> cur $ recordShutdown time
--         -- Thread info
--         CreateThread tid           -> recordThreadCreation tid time
--         (finishThread -> Just tid) -> recordThreadFinish tid time
--         -- Start/end events
--         ThreadLabel tid l          -> labelThread tid l
--         (startId -> Just eid)      -> do cur $ ifInWindow $ recordEventStart eid time
--                                          when (isWindowEvent eid) $ recordWindowStart time
--         (stopId  -> Just eid)      -> do when (isWindowEvent eid) $ recordWindowStop opts time
--                                          cur $ ifInWindow $ recordEventStop eid time
--         _                          -> return ()

--     startId :: EventInfo -> Maybe EventId
--     startId (RunThread tid)                                   = Just $ EventThread tid
--     startId StartGC                                           = Just $ EventGC
--     startId (UserMessage (T.stripPrefix optionsUserStart -> Just e)) = Just $ parseUserEvent e
--     startId _                                                 = Nothing

--     stopId :: EventInfo -> Maybe EventId
--     stopId (StopThread tid _)                               = Just $ EventThread tid
--     stopId EndGC                                            = Just $ EventGC
--     stopId (UserMessage (T.stripPrefix optionsUserStop -> Just e)) = Just $ parseUserEvent e
--     stopId _                                                = Nothing

--     nonEmptyTail :: NonEmpty a -> NonEmpty a
--     nonEmptyTail (_ :| (x : xs)) = x :| xs
--     nonEmptyTail xs              = xs

-- sortedEvents :: EventLog -> [Event]
-- sortedEvents (EventLog _header (Events.Data es)) = Events.sortEvents es
