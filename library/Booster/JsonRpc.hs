{- |
Module      : Booster.JsonRpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.JsonRpc (
    runServer,
) where

import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, throwTo)
import Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import Control.Exception (ErrorCall (..), mask)
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch, MonadMask, catch, handle)
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack (LogLevel (LevelError), MonadLoggerIO)
import Control.Monad.Logger.CallStack qualified as Log
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Value (..))
import Data.Conduit.Network (serverSettings)
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.JSONRPC (
    BatchRequest (BatchRequest, SingleRequest),
    BatchResponse (BatchResponse, SingleResponse),
    ErrorObj (..),
    JSONRPCT,
    Request (..),
    Respond,
    Response (ResponseError),
    Ver (V2),
    buildResponse,
    fromRequest,
    receiveBatchRequest,
    sendBatchResponse,
 )
import Numeric.Natural

import Booster.Definition.Base (KoreDefinition (..))
import Booster.JsonRpc.Base
import Booster.LLVM.Internal qualified as LLVM
import Booster.Network.JsonRpc (jsonrpcTCPServer)
import Booster.Pattern.Base (Pattern)
import Booster.Pattern.Rewrite (RewriteResult (..), performRewrite)
import Booster.Syntax.Json (KoreJson (..), addHeader)
import Booster.Syntax.Json.Base (Id (..))
import Booster.Syntax.Json.Externalise (externalisePattern)
import Booster.Syntax.Json.Internalise (PatternError, internalisePattern)
import Booster.Syntax.ParsedKore (parseKoreDefinition)
import Booster.Syntax.ParsedKore.Base (ParsedDefinition (..), ParsedModule (..))
import Booster.Syntax.ParsedKore.Internalise (DefinitionError (..), addToDefinitions)

respond ::
    forall m.
    MonadMask m =>
    MonadLoggerIO m =>
    MVar ServerState ->
    Maybe LLVM.API ->
    Respond (API 'Req) m (API 'Res)
respond stateVar mLlvmLibrary =
    catchingServerErrors . \case
        Execute req
            | isJust req.stepTimeout -> unsupportedField "step-timeout"
            | isJust req.movingAverageStepTimeout -> unsupportedField "moving-average-step-timeout"
        Execute req -> withMainModule req._module $ \def -> do
            -- internalise given constrained term
            let internalised = runExcept $ internalisePattern Nothing def req.state.term

            case internalised of
                Left patternError -> do
                    Log.logDebug $ "Error internalising cterm" <> Text.pack (show patternError)
                    pure $ Left $ reportPatternError patternError
                Right pat -> do
                    let cutPoints = fromMaybe [] req.cutPointRules
                        terminals = fromMaybe [] req.terminalRules
                        mbDepth = fmap getNat req.maxDepth
                    execResponse <$> performRewrite def mLlvmLibrary mbDepth cutPoints terminals pat
        AddModule req -> do
            -- block other request executions while modifying the server state
            state <- liftIO $ takeMVar stateVar
            let abortWith err = do
                    liftIO (putMVar stateVar state)
                    pure $ Left $ reportDefinitionError err

            case parseKoreDefinition (Text.unpack req.name) req._module of
                Left errMsg ->
                    abortWith $ ParseError (Text.pack errMsg)
                -- we expect exactly one module in the source
                Right ParsedDefinition{modules = []} ->
                    abortWith $ ParseError "Found no modules in input"
                Right ParsedDefinition{modules = _ : _ : _} ->
                    abortWith $ ParseError "Found more than one module in input"
                Right ParsedDefinition{modules = [newModule]}
                    | newModule.name.getId /= req.name ->
                        abortWith $ ParseError ("Module name mismatch: expected " <> req.name)
                    | otherwise ->
                        case runExcept (addToDefinitions newModule state.definitions) of
                            Left err -> abortWith err
                            Right newDefinitions -> do
                                liftIO $ putMVar stateVar state{definitions = newDefinitions}
                                Log.logInfo $ "Added a new module. Now in scope: " <> Text.intercalate ", " (Map.keys newDefinitions)
                                pure $ Right $ AddModule ()

        -- this case is only reachable if the cancel appeared as part of a batch request
        Cancel -> pure $ Left $ ErrorObj "Cancel request unsupported in batch mode" (-32001) Null
        -- using "Method does not exist" error code
        _ -> pure $ Left $ ErrorObj "Not implemented" (-32601) Null
  where
    withMainModule ::
        Maybe Text ->
        (KoreDefinition -> m (Either ErrorObj (API 'Res))) ->
        m (Either ErrorObj (API 'Res))
    withMainModule mbMainModule action = do
        state <- liftIO $ readMVar stateVar
        let mainName = fromMaybe state.defaultMain mbMainModule
        case Map.lookup mainName state.definitions of
            Nothing -> pure $ Left $ reportDefinitionError $ NoSuchModule mainName
            Just d -> action d

    execResponse :: (Natural, RewriteResult Pattern) -> Either ErrorObj (API 'Res)
    execResponse (_, RewriteSingle{}) =
        error "Single rewrite result"
    execResponse (d, RewriteBranch p nexts) =
        Right $
            Execute
                ExecuteResult
                    { reason = Branching
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Just $ map toExecState $ toList nexts
                    , rule = Nothing
                    }
    execResponse (d, RewriteStuck p) =
        Right $
            Execute
                ExecuteResult
                    { reason = Stuck
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Nothing
                    , rule = Nothing
                    }
    execResponse (d, RewriteCutPoint lbl p next) =
        Right $
            Execute
                ExecuteResult
                    { reason = CutPointRule
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Just [toExecState next]
                    , rule = Just lbl
                    }
    execResponse (d, RewriteTerminal lbl p) =
        Right $
            Execute
                ExecuteResult
                    { reason = TerminalRule
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Nothing
                    , rule = Just lbl
                    }
    execResponse (d, RewriteStopped p) =
        Right $
            Execute
                ExecuteResult
                    { reason = DepthBound
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Nothing
                    , rule = Nothing
                    }
    execResponse (d, RewriteAborted p) =
        Right $
            Execute
                ExecuteResult
                    { reason = Aborted
                    , depth = Depth d
                    , state = toExecState p
                    , nextStates = Nothing
                    , rule = Nothing
                    }

    toExecState :: Pattern -> ExecuteState
    toExecState pat =
        ExecuteState{term = addHeader t, predicate = fmap addHeader p}
      where
        (t, p) = externalisePattern pat

    reportPatternError :: PatternError -> ErrorObj
    reportPatternError pErr =
        ErrorObj "Could not verify KORE pattern" (-32002) $ toJSON pErr

    reportDefinitionError :: DefinitionError -> ErrorObj
    reportDefinitionError defErr =
        ErrorObj "Definition error" (-32101) $ toJSON defErr

    unsupportedField name = pure $ Left $ ErrorObj ("Unsupported option: " <> name) (-32100) Null

{- | Catches all calls to `error` from the guts of the engine, and
     returns json with the message and location as context.
-}
catchingServerErrors ::
    MonadCatch m =>
    MonadLoggerIO m =>
    m (Either ErrorObj res) ->
    m (Either ErrorObj res)
catchingServerErrors =
    let mkError (ErrorCallWithLocation msg loc) =
            object ["error" .= msg, "context" .= loc]
     in handle $ \err -> do
            Log.logError $ "Server error: " <> Text.pack (show err)
            pure $ Left (ErrorObj "Server error" (-32032) $ mkError err)

runServer ::
    Int ->
    Map Text KoreDefinition ->
    Text ->
    Maybe LLVM.API ->
    (LogLevel, [LogLevel]) ->
    IO ()
runServer port definitions defaultMain mLlvmLibrary (logLevel, customLevels) =
    do
        stateVar <- newMVar ServerState{definitions, defaultMain}
        Log.runStderrLoggingT . Log.filterLogger levelFilter $
            jsonrpcTCPServer
                rpcJsonConfig
                V2
                False
                srvSettings
                (srv stateVar mLlvmLibrary)
  where
    levelFilter _source lvl =
        lvl `elem` customLevels || lvl >= logLevel && lvl <= LevelError

    srvSettings = serverSettings port "*"

data ServerState = ServerState
    { definitions :: Map Text KoreDefinition
    -- ^ definitions for each loaded module as main module
    , defaultMain :: Text
    -- ^ default main module (initially from command line, could be changed later)
    }

srv ::
    MonadLoggerIO m =>
    MVar ServerState ->
    Maybe LLVM.API ->
    JSONRPCT m ()
srv stateVar mLlvmLibrary = do
    reqQueue <- liftIO $ atomically newTChan
    let mainLoop tid =
            receiveBatchRequest >>= \case
                Nothing -> do
                    return ()
                Just (SingleRequest req) | Right (Cancel :: API 'Req) <- fromRequest req -> do
                    Log.logInfoN "Cancel Request"
                    liftIO $ throwTo tid CancelRequest
                    mainLoop tid
                Just req -> do
                    Log.logInfoN $ Text.pack (show req) -- FIXME reduce logged payload data here!
                    liftIO $ atomically $ writeTChan reqQueue req
                    mainLoop tid
    spawnWorker reqQueue >>= mainLoop
  where
    isRequest = \case
        Request{} -> True
        _ -> False

    cancelError = ErrorObj "Request cancelled" (-32000) Null

    bracketOnReqException before onCancel thing =
        mask $ \restore -> do
            a <- before
            restore (thing a) `catch` \(_ :: ReqException) -> onCancel a

    spawnWorker reqQueue = do
        rpcSession <- ask
        logger <- Log.askLoggerIO
        let withLog :: Log.LoggingT IO a -> IO a
            withLog = flip Log.runLoggingT logger

            sendResponses :: BatchResponse -> Log.LoggingT IO ()
            sendResponses r = flip Log.runLoggingT logger $ flip runReaderT rpcSession $ sendBatchResponse r
            respondTo :: Request -> Log.LoggingT IO (Maybe Response)
            respondTo = buildResponse (respond stateVar mLlvmLibrary)

            cancelReq :: BatchRequest -> Log.LoggingT IO ()
            cancelReq = \case
                SingleRequest req@Request{} -> do
                    let reqVersion = getReqVer req
                        reqId = getReqId req
                    sendResponses $ SingleResponse $ ResponseError reqVersion cancelError reqId
                SingleRequest Notif{} -> pure ()
                BatchRequest reqs -> do
                    sendResponses $ BatchResponse $ [ResponseError (getReqVer req) cancelError (getReqId req) | req <- reqs, isRequest req]

            processReq :: BatchRequest -> Log.LoggingT IO ()
            processReq = \case
                SingleRequest req -> do
                    rM <- respondTo req
                    mapM_ (sendResponses . SingleResponse) rM
                BatchRequest reqs -> do
                    rs <- catMaybes <$> mapM respondTo reqs
                    sendResponses $ BatchResponse rs

        liftIO $
            forkIO $
                forever $
                    bracketOnReqException
                        (atomically $ readTChan reqQueue)
                        (withLog . cancelReq)
                        (withLog . processReq)
