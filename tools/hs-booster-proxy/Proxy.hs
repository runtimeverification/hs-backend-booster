{-# LANGUAGE RankNTypes #-}

{- |
Module      : Proxy
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Proxy (
    KoreServer (..),
    BoosterServer (..),
    serverError,
    respondEither,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Logger qualified as Log
import Data.Aeson.Types (Value (..))
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.JSONRPC
import SMT qualified

import Booster.Definition.Base (KoreDefinition)
import Booster.LLVM.Internal qualified as LLVM

import Kore.Attribute.Symbol (StepperAttributes)
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.JsonRpc (ServerState)
import Kore.JsonRpc.Types
import Kore.Log qualified
import Kore.Syntax.Definition (SentenceAxiom)
import Kore.Syntax.Json.Types qualified as KoreJson

data KoreServer = KoreServer
    { serverState :: MVar.MVar ServerState
    , mainModule :: Text
    , runSMT ::
        forall a.
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        SMT.SMT a ->
        IO a
    , loggerEnv :: Kore.Log.LoggerEnv IO
    }

data BoosterServer = BoosterServer
    { definition :: KoreDefinition
    , mLlvmLibrary :: Maybe LLVM.API
    }

serverError :: String -> Value -> ErrorObj
serverError detail = ErrorObj ("Server error: " <> detail) (-32032)


-- srv :: KoreServer -> BoosterServer -> JSONRPCT (Log.LoggingT IO) ()
-- srv kore@KoreServer{runSMT} booster = do
--     reqQueue <- liftIO $ atomically newTChan
--     let mainLoop tid =
--             receiveBatchRequest >>= \case
--                 Nothing -> do
--                     return ()
--                 Just (SingleRequest req) | Right (Cancel :: API 'Req) <- fromRequest req -> do
--                     Log.logInfoNS "proxy" "Cancel Request"
--                     liftIO $ throwTo tid CancelRequest
--                     mainLoop tid
--                 Just req -> do
--                     Log.logInfoNS "proxy" . (<> "...") . Text.take 200 . Text.pack $ show req
--                     liftIO $ atomically $ writeTChan reqQueue req
--                     mainLoop tid
--     spawnWorker reqQueue >>= mainLoop
--   where
--     isRequest = \case
--         Request{} -> True
--         _ -> False

--     cancelError = ErrorObj "Request cancelled" (-32000) Null

--     bracketOnReqException withLog before onError thing =
--         mask $ \restore -> do
--             a <- before
--             (restore (thing a) `catch` \(_ :: ReqException) -> onError cancelError a)
--                 `catch` \(err :: SomeException) -> withLog (Log.logInfoNS "proxy" (Text.pack $ show err)) >> onError (serverError "crashed" $ toJSON $ show err) a

--     spawnWorker reqQueue = do
--         rpcSession <- ask
--         logger <- Log.askLoggerIO
--         let withLog :: Log.LoggingT IO a -> IO a
--             withLog = flip Log.runLoggingT logger

--             sendResponses :: BatchResponse -> Log.LoggingT IO ()
--             sendResponses r = flip Log.runLoggingT logger $ flip runReaderT rpcSession $ sendBatchResponse r

--             respondTo :: Request -> Log.LoggingT IO (Maybe Response)
--             respondTo req@(Request v _ _ i) =
--                 case fromRequest req of
--                     Left e -> pure . Just $ ResponseError v e i
--                     Right parsed ->
--                         respondEither v i boosterRespond koreRespond parsed
--             respondTo _ = pure Nothing

--             boosterRespond = Booster.respond booster.definition booster.mLlvmLibrary
--             koreRespond = Kore.respond kore.serverState (Kore.ModuleName kore.mainModule) runSMT

--             -- cancelReq :: BatchRequest -> Log.LoggingT IO ()
--             onError err = \case
--                 SingleRequest req@Request{} -> do
--                     let reqVersion = getReqVer req
--                         reqId = getReqId req
--                     sendResponses $ SingleResponse $ ResponseError reqVersion err reqId
--                 SingleRequest Notif{} -> pure ()
--                 BatchRequest reqs -> do
--                     sendResponses $ BatchResponse $ [ResponseError (getReqVer req) err (getReqId req) | req <- reqs, isRequest req]

--             processReq :: BatchRequest -> Log.LoggingT IO ()
--             processReq = \case
--                 SingleRequest req -> do
--                     rM <- respondTo req
--                     mapM_ (sendResponses . SingleResponse) rM
--                 BatchRequest reqs -> do
--                     rs <- catMaybes <$> mapM respondTo reqs
--                     sendResponses $ BatchResponse rs

--         liftIO $
--             forkIO $
--                 forever $
--                     bracketOnReqException
--                         withLog
--                         (atomically $ readTChan reqQueue)
--                         (\e x -> withLog $ onError e x)
--                         (withLog . processReq)

respondEither ::
    forall m.
    Log.MonadLogger m =>
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res)
respondEither booster kore req = case req of
    Execute execReq
        | isJust execReq._module -> kore req
        | isJust execReq.stepTimeout -> kore req
        | isJust execReq.movingAverageStepTimeout -> kore req
        | otherwise -> loop 0 execReq
    Implies _ -> loggedKore "Implies" req
    Simplify _ -> loggedKore "Simplify" req
    AddModule _ ->loggedKore "AddModule" req -- FIXME should go to both
    Cancel -> pure $ Left $ ErrorObj "Cancel not supported" (-32601) Null
  where
    loggedKore msg r = Log.logInfoNS "proxy" (msg <> " (using kore)") >> kore r

    toRequestState :: ExecuteState -> KoreJson.KoreJson
    toRequestState ExecuteState{term = t, substitution, predicate} =
        let subAndPred = catMaybes [KoreJson.term <$> substitution, KoreJson.term <$> predicate]
         in t{KoreJson.term = foldr (KoreJson.KJAnd $ KoreJson.SortApp (KoreJson.Id "SortGeneratedTopCell") []) t.term subAndPred}

    -- loop :: Depth -> ExecuteRequest -> m (Either Response)
    loop currentDepth r = do
        Log.logInfoNS "proxy" . Text.pack $
            "Iterating execute request at " <> show currentDepth
        let mbDepthLimit = flip (-) currentDepth <$> r.maxDepth
         in booster (Execute r{maxDepth = mbDepthLimit}) >>= \case
                Right (Execute boosterResult)
                    -- if the new backend aborts or gets stuck, revert to the old one
                    --
                    -- if we are stuck in the new backend we try to re-run
                    -- in the old one to work around any potential
                    -- unification bugs.
                    | boosterResult.reason `elem` [Aborted, Stuck] -> do
                        -- attempt to do one step in the old backend
                        Log.logInfoNS "proxy" . Text.pack $
                            "Booster " <> show boosterResult.reason <> " at " <> show boosterResult.depth
                        kore
                            ( Execute
                                r
                                    { state = toRequestState boosterResult.state
                                    , maxDepth = Just $ Depth 1
                                    }
                            )
                            >>= \case
                                Right (Execute koreResult)
                                    | koreResult.reason == DepthBound -> do
                                        -- if we made one step, add the number of
                                        -- steps we have taken to the counter and
                                        -- attempt with booster again
                                        Log.logInfoNS "proxy" "kore depth-bound, continuing"
                                        loop
                                            (currentDepth + boosterResult.depth + koreResult.depth)
                                            (r{state = toRequestState koreResult.state} :: ExecuteRequest)
                                    | otherwise -> do
                                        -- otherwise we have hit a different
                                        -- HaltReason, at which point we should
                                        -- return, setting the correct depth
                                        Log.logInfoNS "proxy" . Text.pack $
                                            "Kore " <> show koreResult.reason
                                        pure $
                                            Right $
                                                Execute koreResult{depth = currentDepth + boosterResult.depth + koreResult.depth}
                                -- can only be an error at this point
                                res -> pure res
                    | otherwise -> do
                        -- we were successful with the booster, thus we
                        -- return the booster result with the updated
                        -- depth, in case we previously looped
                        Log.logInfoNS "proxy" . Text.pack $
                            "Booster " <> show boosterResult.reason <> " at " <> show boosterResult.depth
                        pure $ Right $ Execute boosterResult{depth = currentDepth + boosterResult.depth}
                -- can only be an error at this point
                res -> pure res
