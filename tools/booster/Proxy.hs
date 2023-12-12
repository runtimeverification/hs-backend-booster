{-# LANGUAGE RankNTypes #-}

{- |
Module      : Proxy
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Proxy (
    KoreServer (..),
    ProxyConfig (..),
    serverError,
    respondEither,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Catch (MonadCatch (..), catch)
import Control.Monad.Logger qualified as Log
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Except (runExcept)
import Data.Aeson (ToJSON (..))
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types (Value (..))
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.JSONRPC
import System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime, toNanoSecs)

import Booster.Definition.Base (KoreDefinition)
import Booster.JsonRpc as Booster (ServerState (..), execStateToKoreJson, toExecState)
import Booster.JsonRpc.Utils (diffBy)
import Booster.Syntax.Json.Internalise
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.JsonRpc qualified as Kore (ServerState)
import Kore.JsonRpc.Types
import Kore.JsonRpc.Types qualified as ExecuteRequest (ExecuteRequest (..))
import Kore.JsonRpc.Types qualified as SimplifyRequest (SimplifyRequest (..))
import Kore.JsonRpc.Types.Log qualified as RPCLog
import Kore.Log qualified
import Kore.Log.DecidePredicateUnknown (DecidePredicateUnknown, externaliseDecidePredicateUnknown)
import Kore.Syntax.Definition (SentenceAxiom)
import Kore.Syntax.Json.Types qualified as KoreJson
import SMT qualified
import Stats (StatsVar, addStats, microsWithUnit, timed)

data KoreServer = KoreServer
    { serverState :: MVar.MVar Kore.ServerState
    , mainModule :: Text
    , runSMT ::
        forall a.
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        SMT.SMT a ->
        IO a
    , loggerEnv :: Kore.Log.LoggerEnv IO
    }

data ProxyConfig = ProxyConfig
    { statsVar :: Maybe StatsVar
    , forceFallback :: Maybe Depth
    , boosterState :: MVar.MVar Booster.ServerState
    }

serverError :: String -> Value -> ErrorObj
serverError detail = ErrorObj ("Server error: " <> detail) (-32032)

data ExecutionLoopParams = ExecutionLoopParams
    { logSettings :: !LogSettings
    , definition :: !KoreDefinition
    }

data ExecuteLoopState = ExecuteLoopState
    { depth :: !Depth
    , mforceSimplification :: !(Maybe Depth)
    , time :: !Double
    , koreTime :: !Double
    , rpcLogs :: !(Maybe [RPCLog.LogEntry])
    }

data LogSettings = LogSettings
    { logSuccessfulSimplifications :: !(Maybe Bool)
    , logFailedSimplifications :: !(Maybe Bool)
    , logSuccessfulRewrites :: !(Maybe Bool)
    , logFailedRewrites :: !(Maybe Bool)
    , logFallbacks :: !(Maybe Bool)
    , logTiming :: !(Maybe Bool)
    }

respondEither ::
    forall m.
    Log.MonadLogger m =>
    (MonadIO m, MonadCatch m) =>
    ProxyConfig ->
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res)
respondEither ProxyConfig{statsVar, forceFallback, boosterState} booster kore req = case req of
    Execute execReq
        | isJust execReq.stepTimeout || isJust execReq.movingAverageStepTimeout ->
            loggedKore ExecuteM req
        | otherwise ->
            let logSettings =
                    LogSettings
                        { logSuccessfulSimplifications = execReq.logSuccessfulSimplifications
                        , logFailedSimplifications = execReq.logFailedSimplifications
                        , logSuccessfulRewrites = execReq.logSuccessfulRewrites
                        , logFailedRewrites = execReq.logFailedRewrites
                        , logFallbacks = execReq.logFallbacks
                        , logTiming = execReq.logTiming
                        }
             in liftIO (getTime Monotonic) >>= \start -> do
                    bState <- liftIO $ MVar.readMVar boosterState
                    let m = fromMaybe bState.defaultMain execReq._module
                        def =
                            fromMaybe (error $ "Module " <> show m <> " not found") $
                                Map.lookup m bState.definitions
                    handleExecute logSettings def execReq
                        >>= traverse
                            ( flip runReaderT ExecutionLoopParams{logSettings, definition = def}
                                . postExecSimplify
                                    start
                                    execReq._module
                            )
    Implies _ ->
        loggedKore ImpliesM req
    Simplify simplifyReq ->
        liftIO (getTime Monotonic) >>= handleSimplify simplifyReq . Just
    AddModule _ -> do
        -- execute in booster first, assuming that kore won't throw an
        -- error if booster did not. The response is empty anyway.
        (boosterResult, boosterTime) <- Stats.timed $ booster req
        case boosterResult of
            Left _err -> pure boosterResult
            Right _ -> do
                (koreRes, koreTime) <- Stats.timed $ kore req
                logStats AddModuleM (boosterTime + koreTime, koreTime)
                pure koreRes
    GetModel _ ->
        loggedKore GetModelM req
    Cancel ->
        pure $ Left $ ErrorObj "Cancel not supported" (-32601) Null
  where
    handleSimplify :: SimplifyRequest -> Maybe TimeSpec -> m (Either ErrorObj (API 'Res))
    handleSimplify simplifyReq mbStart = do
        -- execute in booster first, then in kore. Log the difference
        (boosterResult, boosterTime) <-
            Stats.timed $ booster (Simplify simplifyReq)
        case boosterResult of
            Right (Simplify boosterRes) -> do
                let koreReq =
                        Simplify
                            simplifyReq
                                { SimplifyRequest.state = boosterRes.state
                                }
                (koreResult, koreTime) <- Stats.timed $ kore koreReq
                case koreResult of
                    Right (Simplify koreRes) -> do
                        logStats SimplifyM (boosterTime + koreTime, koreTime)
                        when (koreRes.state /= boosterRes.state) $ do
                            bState <- liftIO (MVar.readMVar boosterState)
                            let m = fromMaybe bState.defaultMain simplifyReq._module
                                def =
                                    fromMaybe (error $ "Module " <> show m <> " not found") $
                                        Map.lookup m bState.definitions
                            Log.logOtherNS "proxy" (Log.LevelOther "Simplify") $
                                let diff =
                                        fromMaybe "<syntactic difference only>" $
                                            diffBy def boosterRes.state.term koreRes.state.term
                                 in Text.pack ("Kore simplification: Diff (< before - > after)\n" <> diff)
                        stop <- liftIO $ getTime Monotonic
                        let timing
                                | Just start <- mbStart
                                , fromMaybe False simplifyReq.logTiming =
                                    Just
                                        [ RPCLog.ProcessingTime
                                            Nothing
                                            (fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9)
                                        ]
                                | otherwise = Nothing
                        pure . Right . Simplify $
                            SimplifyResult
                                { state = koreRes.state
                                , logs = combineLogs [timing, boosterRes.logs, koreRes.logs]
                                }
                    koreError ->
                        -- can only be an error
                        pure koreError
            Left ErrorObj{getErrMsg, getErrData} -> do
                -- in case of problems, log the problem and try with kore
                let boosterError
                        | Object errObj <- getErrData
                        , Just err <- Aeson.lookup "error" errObj =
                            fromString err
                        | otherwise = fromString getErrData
                    fromString (String s) = s
                    fromString other = Text.pack (show other)
                Log.logWarnNS "proxy" . Text.unwords $
                    ["Problem with simplify request: ", Text.pack getErrMsg, "-", boosterError]
                -- NB the timing information for booster execution is lost here.
                loggedKore SimplifyM req
            _wrong ->
                pure . Left $ ErrorObj "Wrong result type" (-32002) $ toJSON _wrong

    loggedKore method r = do
        Log.logInfoNS "proxy" . Text.pack $ show method <> " (using kore)"
        (result, time) <- Stats.timed $ kore r
        logStats method (time, time)
        pure result

    logStats method (time, koreTime)
        | Just v <- statsVar = do
            addStats v method time koreTime
            Log.logInfoNS "proxy" . Text.pack . unwords $
                [ "Performed"
                , show method
                , "in"
                , microsWithUnit time
                , "(" <> microsWithUnit koreTime <> " kore time)"
                ]
        | otherwise =
            pure ()

    handleExecute :: LogSettings -> KoreDefinition -> ExecuteRequest -> m (Either ErrorObj (API 'Res))
    handleExecute logSettings def =
        flip runReaderT ExecutionLoopParams{logSettings, definition = def}
            . flip
                evalStateT
                ExecuteLoopState
                    { depth = 0
                    , mforceSimplification = forceFallback
                    , time = 0.0
                    , koreTime = 0.0
                    , rpcLogs = Nothing
                    }
            . executionLoop

    executionLoop ::
        ExecuteRequest ->
        StateT ExecuteLoopState (ReaderT ExecutionLoopParams m) (Either ErrorObj (API 'Res))
    executionLoop r = do
        ExecutionLoopParams{logSettings} <- ask
        loopState@ExecuteLoopState{depth = currentDepth@(Depth cDepth), mforceSimplification, time, koreTime, rpcLogs} <-
            get
        Log.logInfoNS "proxy" . Text.pack $
            if currentDepth == 0
                then "Starting execute request"
                else "Iterating execute request at " <> show currentDepth
        -- calculate depth limit, considering possible forced Kore simplification
        let mbDepthLimit = case (mforceSimplification, r.maxDepth) of
                (Just (Depth forceDepth), Just (Depth maxDepth)) ->
                    if cDepth + forceDepth < maxDepth
                        then Just $ Depth forceDepth
                        else Just $ Depth $ maxDepth - cDepth
                (Just forceDepth, _) -> Just forceDepth
                (_, Just maxDepth) -> Just $ maxDepth - currentDepth
                _ -> Nothing
        (bResult, bTime) <- lift $ lift $ Stats.timed $ booster (Execute r{maxDepth = mbDepthLimit})
        case bResult of
            Right (Execute boosterResult)
                -- the execution reached the depth bound due to a forced Kore simplification
                | boosterResult.reason == DepthBound && isJust mforceSimplification -> do
                    Log.logInfoNS "proxy" . Text.pack $
                        "Forced simplification at " <> show (currentDepth + boosterResult.depth)
                    simplifyResult <- lift $ simplifyExecuteState r._module boosterResult.state
                    case simplifyResult of
                        Left logsOnly -> do
                            -- state was simplified to \bottom, return vacuous
                            Log.logInfoNS "proxy" "Vacuous state after simplification"
                            pure . Right . Execute $ makeVacuous logsOnly boosterResult
                        Right (simplifiedBoosterState, boosterStateSimplificationLogs) -> do
                            put
                                loopState
                                    { depth = currentDepth + boosterResult.depth
                                    , time = time + bTime
                                    , koreTime
                                    , rpcLogs =
                                        combineLogs
                                            [ rpcLogs
                                            , boosterResult.logs
                                            , boosterStateSimplificationLogs
                                            ]
                                    }
                            executionLoop
                                r{ExecuteRequest.state = execStateToKoreJson simplifiedBoosterState}
                -- if the new backend aborts, branches or gets stuck, revert to the old one
                --
                -- if we are stuck in the new backend we try to re-run
                -- in the old one to work around any potential
                -- unification bugs.
                | boosterResult.reason `elem` [Aborted, Stuck, Branching] -> do
                    Log.logInfoNS "proxy" . Text.pack $
                        "Booster " <> show boosterResult.reason <> " at " <> show boosterResult.depth
                    -- simplify Booster's state with Kore's simplifier
                    Log.logInfoNS "proxy" . Text.pack $ "Simplifying booster state and falling back to Kore "
                    simplifyResult <- lift $ simplifyExecuteState r._module boosterResult.state
                    case simplifyResult of
                        Left logsOnly -> do
                            -- state was simplified to \bottom, return vacuous
                            Log.logInfoNS "proxy" "Vacuous state after simplification"
                            pure . Right . Execute $ makeVacuous logsOnly boosterResult
                        Right (simplifiedBoosterState, boosterStateSimplificationLogs) -> do
                            -- attempt to do one step in the old backend
                            (kResult, kTime) <-
                                lift $
                                    lift $
                                        Stats.timed $
                                            kore
                                                ( Execute
                                                    r
                                                        { state = execStateToKoreJson simplifiedBoosterState
                                                        , maxDepth = Just $ Depth 1
                                                        }
                                                )
                            when (isJust statsVar) $
                                Log.logInfoNS "proxy" . Text.pack $
                                    "Kore fall-back in " <> microsWithUnit kTime
                            case kResult of
                                Right (Execute koreResult) -> do
                                    let
                                        fallbackLog =
                                            if fromMaybe False logSettings.logFallbacks
                                                then Just [mkFallbackLogEntry boosterResult koreResult]
                                                else Nothing
                                    case koreResult.reason of
                                        DepthBound -> do
                                            -- if we made one step, add the number of
                                            -- steps we have taken to the counter and
                                            -- attempt with booster again
                                            when (koreResult.depth == 0) $ error "Expected kore-rpc to take at least one step"
                                            Log.logInfoNS "proxy" $
                                                Text.pack $
                                                    "kore depth-bound, continuing... (currently at "
                                                        <> show (currentDepth + boosterResult.depth + koreResult.depth)
                                                        <> ")"
                                            put
                                                loopState
                                                    { depth = currentDepth + boosterResult.depth + koreResult.depth
                                                    , time = time + bTime + kTime
                                                    , koreTime = koreTime + kTime
                                                    , rpcLogs =
                                                        combineLogs
                                                            [ rpcLogs
                                                            , boosterResult.logs
                                                            , boosterStateSimplificationLogs
                                                            , koreResult.logs
                                                            , fallbackLog
                                                            ]
                                                    }
                                            executionLoop
                                                r{ExecuteRequest.state = execStateToKoreJson koreResult.state}
                                        _ -> do
                                            -- otherwise we have hit a different
                                            -- HaltReason, at which point we should
                                            -- return, setting the correct depth
                                            Log.logInfoNS "proxy" . Text.pack $
                                                "Kore " <> show koreResult.reason <> " at " <> show koreResult.depth
                                            lift $ lift $ logStats ExecuteM (time + bTime + kTime, koreTime + kTime)
                                            pure $
                                                Right $
                                                    Execute
                                                        koreResult
                                                            { depth = currentDepth + boosterResult.depth + koreResult.depth
                                                            , logs =
                                                                combineLogs
                                                                    [ rpcLogs
                                                                    , boosterResult.logs
                                                                    , koreResult.logs
                                                                    , fallbackLog
                                                                    ]
                                                            }
                                -- can only be an error at this point
                                res -> pure res
                | otherwise -> do
                    -- we were successful with the booster, thus we
                    -- return the booster result with the updated
                    -- depth, in case we previously looped
                    Log.logInfoNS "proxy" . Text.pack $
                        "Booster " <> show boosterResult.reason <> " at " <> show boosterResult.depth
                    lift $ lift $ logStats ExecuteM (time + bTime, koreTime)
                    pure $
                        Right $
                            Execute
                                boosterResult
                                    { depth = currentDepth + boosterResult.depth
                                    , logs = combineLogs [rpcLogs, boosterResult.logs]
                                    }
            -- can only be an error at this point
            res -> pure res

    -- Performs an internal simplify call for the given execute state.
    -- If the state simplifies to bottom, only the logs are returned,
    -- otherwise the logs and the simplified state (after splitting it
    -- into term and predicates by an internal trivial execute call).
    simplifyExecuteState ::
        Maybe Text ->
        ExecuteState ->
        ReaderT
            ExecutionLoopParams
            m
            (Either (Maybe [RPCLog.LogEntry]) (ExecuteState, Maybe [RPCLog.LogEntry]))
    simplifyExecuteState
        mbModule
        s = do
            params <- ask
            Log.logInfoNS "proxy" "Simplifying execution state"
            simplResult <-
                lift $ handleSimplify (toSimplifyRequest params s) Nothing
            case simplResult of
                -- This request should not fail, as the only possible
                -- failure mode would be malformed or invalid kore
                Right (Simplify simplified)
                    | KoreJson.KJBottom _ <- simplified.state.term ->
                        pure (Left simplified.logs)
                    | otherwise -> do
                        -- convert back to a term/constraints form by re-internalising
                        let internalPattern =
                                runExcept $
                                    internalisePattern
                                        DisallowAlias
                                        IgnoreSubsorts
                                        Nothing
                                        params.definition
                                        simplified.state.term
                        case internalPattern of
                            Right (p, sub) ->
                                pure $ Right (Booster.toExecState p sub, simplified.logs)
                            Left err -> do
                                Log.logWarnNS "proxy" $
                                    "Error processing execute state simplification result: "
                                        <> Text.pack (show err)
                                pure $ Right (s, Nothing)
                _other -> do
                    -- if we hit an error here, return the original
                    Log.logWarnNS "proxy" "Unexpected failure when calling Kore simplifier, returning original term"
                    pure $ Right (s, Nothing)
          where
            toSimplifyRequest :: ExecutionLoopParams -> ExecuteState -> SimplifyRequest
            toSimplifyRequest params state =
                SimplifyRequest
                    { state = execStateToKoreJson state
                    , _module = mbModule
                    , logSuccessfulSimplifications = params.logSettings.logSuccessfulSimplifications
                    , logFailedSimplifications = params.logSettings.logFailedSimplifications
                    , logTiming = params.logSettings.logTiming
                    }

    postExecSimplify ::
        TimeSpec -> Maybe Text -> API 'Res -> ReaderT ExecutionLoopParams m (API 'Res)
    postExecSimplify start mbModule = \case
        Execute res ->
            Execute
                <$> ( simplifyExecuteResult res
                        `catch` ( \(err :: DecidePredicateUnknown) ->
                                    pure res{reason = Aborted, unknownPredicate = Just . externaliseDecidePredicateUnknown $ err}
                                )
                    )
        other -> pure other
      where
        -- timeLog :: Maybe Bool -> TimeDiff -> Maybe [LogEntry]
        timeLog mbLogTiming stop
            | fromMaybe False mbLogTiming =
                let duration =
                        fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9
                 in Just [RPCLog.ProcessingTime Nothing duration]
            | otherwise =
                Nothing

        simplifyExecuteResult :: ExecuteResult -> ReaderT ExecutionLoopParams m ExecuteResult
        simplifyExecuteResult res@ExecuteResult{reason, state, nextStates} = do
            params <- ask
            Log.logInfoNS "proxy" . Text.pack $ "Simplifying state in " <> show reason <> " result"
            simplified <- simplifyExecuteState mbModule state
            case simplified of
                Left logsOnly -> do
                    -- state simplified to \bottom, return vacuous
                    Log.logInfoNS "proxy" "Vacuous after simplifying result state"
                    stop <- liftIO $ getTime Monotonic
                    pure $ makeVacuous (combineLogs [timeLog params.logSettings.logTiming stop, logsOnly]) res
                Right (simplifiedState, simplifiedStateLogs) -> do
                    simplifiedNexts <-
                        maybe
                            (pure [])
                            (mapM $ simplifyExecuteState mbModule)
                            nextStates
                    stop <- liftIO $ getTime Monotonic
                    let (logsOnly, (filteredNexts, filteredNextLogs)) =
                            second unzip $ partitionEithers simplifiedNexts
                        newLogs = simplifiedStateLogs : logsOnly <> filteredNextLogs

                    pure $ case reason of
                        Branching
                            | null filteredNexts ->
                                res
                                    { reason = Stuck
                                    , nextStates = Nothing
                                    , logs =
                                        combineLogs $ timeLog params.logSettings.logTiming stop : res.logs : simplifiedStateLogs : logsOnly
                                    }
                            | length filteredNexts == 1 ->
                                res -- What now? would have to re-loop. Return as-is.
                                -- otherwise falling through to _otherReason
                        CutPointRule
                            | null filteredNexts ->
                                makeVacuous (combineLogs $ timeLog params.logSettings.logTiming stop : newLogs) res
                        _otherReason ->
                            res
                                { state = simplifiedState
                                , nextStates =
                                    if null filteredNexts
                                        then Nothing
                                        else Just filteredNexts
                                , logs = combineLogs $ timeLog params.logSettings.logTiming stop : res.logs : newLogs
                                }

-- | Combine multiple, possibly empty/non-existent (Nothing) lists of logs into one
combineLogs :: [Maybe [RPCLog.LogEntry]] -> Maybe [RPCLog.LogEntry]
combineLogs logSources
    | all isNothing logSources = Nothing
    | otherwise = Just $ concat $ catMaybes logSources

makeVacuous :: Maybe [RPCLog.LogEntry] -> ExecuteResult -> ExecuteResult
makeVacuous newLogs executeState =
    executeState
        { reason = Vacuous
        , nextStates = Nothing
        , rule = Nothing
        , logs = combineLogs [executeState.logs, newLogs]
        }

mkFallbackLogEntry :: ExecuteResult -> ExecuteResult -> RPCLog.LogEntry
mkFallbackLogEntry boosterResult koreResult =
    let boosterRewriteFailureLog = filter isRewriteFailureLogEntry . fromMaybe [] $ boosterResult.logs
        lastBoosterRewriteLogEntry = case boosterRewriteFailureLog of
            [] -> Nothing
            xs -> Just $ last xs
        fallbackRuleId =
            case lastBoosterRewriteLogEntry of
                Nothing -> "UNKNOWN"
                Just logEntry -> fromMaybe "UNKNOWN" $ getRewriteFailureRuleId logEntry
        fallbackReason =
            case lastBoosterRewriteLogEntry of
                Nothing -> "UNKNOWN"
                Just logEntry -> fromMaybe "UNKNOWN" $ getRewriteFailureReason logEntry
        koreRewriteSuccessLog = filter isRewriteSuccessLogEntry . fromMaybe [] $ koreResult.logs
        koreRuleIds = mapMaybe getRewriteSuccessRuleId koreRewriteSuccessLog
     in RPCLog.Fallback
            { originalTerm = Just $ execStateToKoreJson boosterResult.state
            , rewrittenTerm = Just $ execStateToKoreJson koreResult.state
            , reason = fallbackReason
            , fallbackRuleId = fallbackRuleId
            , recoveryRuleIds = NonEmpty.nonEmpty koreRuleIds
            , recoveryDepth = koreResult.depth
            , origin = RPCLog.Proxy
            }
  where
    isRewriteFailureLogEntry :: RPCLog.LogEntry -> Bool
    isRewriteFailureLogEntry = \case
        RPCLog.Rewrite{result = RPCLog.Failure{}} -> True
        _ -> False

    isRewriteSuccessLogEntry :: RPCLog.LogEntry -> Bool
    isRewriteSuccessLogEntry = \case
        RPCLog.Rewrite{result = RPCLog.Success{}} -> True
        _ -> False

    getRewriteFailureReason :: RPCLog.LogEntry -> Maybe Text
    getRewriteFailureReason = \case
        RPCLog.Rewrite{result = RPCLog.Failure{reason}} -> Just reason
        _ -> Nothing

    getRewriteFailureRuleId :: RPCLog.LogEntry -> Maybe Text
    getRewriteFailureRuleId = \case
        RPCLog.Rewrite{result = RPCLog.Failure{_ruleId}} -> _ruleId
        _ -> Nothing

    getRewriteSuccessRuleId :: RPCLog.LogEntry -> Maybe Text
    getRewriteSuccessRuleId = \case
        RPCLog.Rewrite{result = RPCLog.Success{ruleId}} -> Just ruleId
        _ -> Nothing
