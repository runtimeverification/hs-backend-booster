
module Main where
import qualified GHC.RTS.Events as Events
import System.Environment (getArgs)

main :: IO ()
main = do
  [eventlogFile] <- getArgs
  eventlog <- either error id <$> Events.readEventLogFromFile eventlogFile
  pure ()



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