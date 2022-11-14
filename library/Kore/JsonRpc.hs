{- |
Module      : Kore.JsonRpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.JsonRpc (
    runServer,
) where

import Control.Concurrent (forkIO, throwTo)
import Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import Control.Exception (mask)
import Control.Monad (forever)
import Control.Monad.Catch (catch)
import Control.Monad.Logger.CallStack (LogLevel, MonadLoggerIO)
import Control.Monad.Logger.CallStack qualified as Log
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Control.Monad.STM (atomically)
import Data.Aeson.Encode.Pretty as Json
import Data.Aeson.Types (Value (..))
import Data.Conduit.Network (serverSettings)
import Data.Maybe (catMaybes)
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

import Kore.Definition.Base (KoreDefinition (..))
import Kore.JsonRpc.Base
import Kore.Network.JsonRpc (jsonrpcTCPServer)
import Kore.Syntax.Json.Base (Id (..), KORE (..), KoreJson (..), KorePattern (..), Sort (..), Version (..))

respond ::
    forall m.
    MonadLoggerIO m =>
    KoreDefinition ->
    Respond (API 'Req) m (API 'Res)
respond KoreDefinition{} =
    \case
        Execute _ -> do
            Log.logDebug "Testing JSON-RPC server."
            pure $ Right dummyExecuteResult
        -- this case is only reachable if the cancel appeared as part of a batch request
        Cancel -> pure $ Left $ ErrorObj "Cancel request unsupported in batch mode" (-32001) Null
        -- using "Method does not exist" error code
        _ -> pure $ Left $ ErrorObj "Not implemented" (-32601) Null
  where
    dummyExecuteState :: ExecuteState
    dummyExecuteState =
        ExecuteState
            { term = dummyKoreJson
            , predicate = Nothing
            }

    dummyExecuteResult :: API 'Res
    dummyExecuteResult =
        Execute
            ExecuteResult
                { reason = Stuck
                , depth = Depth 0
                , state = dummyExecuteState
                , nextStates = Nothing
                , rule = Nothing
                }

    dummyKoreJson :: KoreJson
    dummyKoreJson =
        KoreJson
            { format = KORE
            , version = KJ1
            , term = KJTop (SortVar (Id "SV"))
            }

runServer :: Int -> KoreDefinition -> LogLevel -> IO ()
runServer port internalizedModule logLevel =
    do
        Log.runStderrLoggingT . logFilter
        $ jsonrpcTCPServer
            Json.defConfig{confCompare}
            V2
            False
            srvSettings
            (srv internalizedModule)
  where
    logFilter = Log.filterLogger $ \_source lvl -> lvl >= logLevel
    srvSettings = serverSettings port "*"
    confCompare =
        Json.keyOrder
            [ "format"
            , "version"
            , "term"
            , "tag"
            , "assoc"
            , "name"
            , "symbol"
            , "argSort"
            , "sort"
            , "sorts"
            , "var"
            , "varSort"
            , "arg"
            , "args"
            , "argss"
            , "source"
            , "dest"
            , "value"
            , "jsonrpc"
            , "id"
            , "reason"
            , "depth"
            , "rule"
            , "state"
            , "next-states"
            , "substitution"
            , "predicate"
            , "satisfiable"
            , "implication"
            , "condition"
            ]

srv :: MonadLoggerIO m => KoreDefinition -> JSONRPCT m ()
srv internalizedModule = do
    reqQueue <- liftIO $ atomically newTChan
    let mainLoop tid =
            receiveBatchRequest >>= \case
                Nothing -> do
                    return ()
                Just (SingleRequest req) | Right (Cancel :: API 'Req) <- fromRequest req -> do
                    Log.logInfoN $ "Cancel Request"
                    liftIO $ throwTo tid CancelRequest
                    mainLoop tid
                Just req -> do
                    Log.logInfoN $ Text.pack (show req)
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
            respondTo :: MonadLoggerIO m => Request -> m (Maybe Response)
            respondTo = buildResponse (respond internalizedModule)

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
