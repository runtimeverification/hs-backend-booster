{- |
Module      : Proxy
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
{-# LANGUAGE RankNTypes #-}

module Proxy (
    KoreServer(..),
    BoosterServer(..),
    runServer,
) where
import  Control.Concurrent.MVar qualified as MVar
import Kore.JsonRpc (ServerState)
import Booster.JsonRpc.Base (ModuleName, rpcJsonConfig, API (..), ReqOrRes (..), ReqException (..), ExecuteResult (..), HaltReason (..))
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.Syntax.Definition (SentenceAxiom)
import Kore.Internal.TermLike (TermLike, VariableName)
import  SMT qualified
import  Kore.Log qualified
import Kore.Syntax.Module qualified as Kore
import Booster.Definition.Base (KoreDefinition)
import Booster.LLVM.Internal qualified as LLVM
import Control.Monad.Logger (LogLevel (..), LogSource)
import qualified Control.Monad.Logger as Log
import Booster.Network.JsonRpc (jsonrpcTCPServer)
import Network.JSONRPC hiding (jsonrpcTCPServer)
import Data.Conduit.Network (serverSettings)
import qualified Data.Text as Text


import Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import Control.Monad.STM (atomically)
-- import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (ask, runReaderT)
-- import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Value (..), ToJSON (toJSON))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (throwTo, forkIO)
import Control.Exception (mask)
import Control.Monad.Catch (catch)
import Data.Maybe (catMaybes, isJust)
import Control.Monad (forever)
import qualified Booster.JsonRpc as Booster
import qualified Kore.JsonRpc as Kore
import qualified Booster.JsonRpc.Base as Booster
import qualified Kore.JsonRpc.Base as Kore


data KoreServer = KoreServer {
    serverState :: MVar.MVar ServerState,
    mainModule :: ModuleName,
    runSMT :: forall a.
      SmtMetadataTools StepperAttributes
                      -> [SentenceAxiom (TermLike VariableName)] -> SMT.SMT a -> IO a,
    loggerEnv :: Kore.Log.LoggerEnv IO
}


data BoosterServer = BoosterServer {
    definition :: KoreDefinition
    , mLlvmLibrary :: Maybe LLVM.API
    , logLevel :: LogLevel
    , customLevels :: [LogLevel]
}

runServer :: Int -> KoreServer -> BoosterServer -> IO ()
runServer port kore booster =
    do
        Log.runFileLoggingT "booster.log" . Log.filterLogger levelFilter
        $ jsonrpcTCPServer
            rpcJsonConfig
            V2
            False
            srvSettings $
            srv kore booster
  where
    levelFilter :: LogSource -> LogLevel -> Bool
    levelFilter _source lvl =
        lvl `elem` booster.customLevels || lvl >= booster.logLevel && lvl <= LevelError

    srvSettings = serverSettings port "*"

srv :: KoreServer -> BoosterServer -> JSONRPCT (Log.LoggingT IO) ()
srv kore@KoreServer{runSMT} booster = do
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
            respondTo :: Request -> Log.LoggingT IO (Maybe Response)
            respondTo req@(Request v _ _ i) =
                case (fromRequest req, fromRequest req) of
                    (Left e, _) -> return . Just $ ResponseError v e i
                    (_, Left e) -> return . Just $ ResponseError v e i
                    (Right (boosterR :: Booster.API 'Booster.Req), Right (koreR :: Kore.API 'Kore.Req)) -> 
                        respondEither v i boosterRespond koreRespond boosterR koreR
            respondTo _ = return Nothing

            boosterRespond = Booster.respond booster.definition booster.mLlvmLibrary
            koreRespond = Kore.respond kore.serverState (Kore.ModuleName kore.mainModule) runSMT

            

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

respondEither :: Monad m =>
    Ver
    -> Id
    -> Respond (Booster.API 'Booster.Req) m (Booster.API 'Booster.Res)
    -> Respond (Kore.API 'Kore.Req) m (Kore.API 'Kore.Res)
    -> Booster.API 'Booster.Req
    -> Kore.API 'Kore.Req
    -> m (Maybe Response)
respondEither v i booster kore boosterR koreR = case boosterR of
    Execute req
        | isJust req._module -> mkResponse =<< kore koreR
        | isJust req.stepTimeout -> mkResponse =<< kore koreR
        | isJust req.movingAverageStepTimeout -> mkResponse =<< kore koreR
    Execute _ -> booster boosterR >>= \case
        Right (Execute (ExecuteResult{reason}))
            -- TODO: we want to make a step in the old backend and then resume here
            | reason == Aborted -> mkResponse =<< kore koreR
            | reason == Stuck -> mkResponse =<< kore koreR
        res -> mkResponse res
    Implies _ -> mkResponse =<< kore koreR
    Simplify _ -> mkResponse =<< kore koreR
    AddModule _ -> mkResponse =<< kore koreR
    Cancel -> mkResponse =<< booster boosterR
    where
        mkResponse :: Monad m => ToJSON a => Either ErrorObj a -> m (Maybe Response)
        mkResponse = \case
                    Left  e -> return . Just $ ResponseError v e i
                    Right r -> return . Just $ Response v (toJSON r) i