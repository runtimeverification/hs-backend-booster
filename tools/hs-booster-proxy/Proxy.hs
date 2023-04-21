{-# LANGUAGE RankNTypes #-}

{- |
Module      : Proxy
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Proxy (
    KoreServer (..),
    ProxyOptions (..),
    serverError,
    respondEither,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Logger qualified as Log
import Data.Aeson (Value (..))
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.JSONRPC
import SMT qualified

import Kore.Attribute.Symbol (StepperAttributes)
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.JsonRpc qualified as Kore (ServerState)
import Kore.JsonRpc.Types
import Kore.JsonRpc.Types qualified as ExecuteRequest (ExecuteRequest (..))
import Kore.Log qualified
import Kore.Syntax.Definition (SentenceAxiom)
import Kore.Syntax.Json.Types qualified as KoreJson

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

serverError :: String -> Value -> ErrorObj
serverError detail = ErrorObj ("Server error: " <> detail) (-32032)

-- | proxy-specific options and dev-options for experimentation
data ProxyOptions = ProxyOptions
    { simplifyAfterExec :: Bool
    -- ^ run kore simplifier on all result terms after executing
    }

respondEither ::
    forall m.
    Log.MonadLogger m =>
    ProxyOptions ->
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res)
respondEither ProxyOptions{simplifyAfterExec} booster kore req = case req of
    Execute execReq
        | isJust execReq.stepTimeout -> kore req
        | isJust execReq.movingAverageStepTimeout -> kore req
        | otherwise -> loop 0 execReq >>= traverse (postExecSimplify execReq._module)
    Implies _ -> loggedKore "Implies" req
    Simplify _ -> loggedKore "Simplify" req
    AddModule _ -> do
        -- execute in booster first, assuming that kore won't throw an
        -- error if booster did not. The response is empty anyway.
        booster req >>= \case
            Left err -> pure $ Left err
            Right _ -> kore req
    Cancel -> pure $ Left $ ErrorObj "Cancel not supported" (-32601) Null
  where
    loggedKore msg r = Log.logInfoNS "proxy" (msg <> " (using kore)") >> kore r

    toRequestState :: ExecuteState -> KoreJson.KoreJson
    toRequestState ExecuteState{term = t, substitution, predicate} =
        let subAndPred = catMaybes [KoreJson.term <$> substitution, KoreJson.term <$> predicate]
            termSort = KoreJson.SortApp (KoreJson.Id "SortGeneratedTopCell") []
         in t{KoreJson.term = foldr (KoreJson.KJAnd termSort) t.term subAndPred}

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
                                            r{ExecuteRequest.state = toRequestState koreResult.state}
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

    postExecSimplify :: Maybe Text -> API 'Res -> m (API 'Res)
    postExecSimplify mbModule
        | not simplifyAfterExec = pure . id
        | otherwise = \case
            Execute res ->
                Execute <$> simplifyResult res
            other -> pure other
      where
        simplifyResult :: ExecuteResult -> m ExecuteResult
        simplifyResult res@ExecuteResult{reason, state, nextStates} = do
            Log.logInfoNS "proxy" . Text.pack $ "Simplifying state in " <> show reason <> " result"
            simplifiedState <- runSimplify state
            simplifiedNexts <- maybe (pure []) (mapM runSimplify) nextStates
            let filteredNexts = filter (not . isBottom) simplifiedNexts
            let result = case reason of
                    Branching
                        | length filteredNexts == 1 ->
                            res -- What now? would have to re-loop. Return as-is.
                        | length filteredNexts == 0 ->
                            res{reason = Stuck, nextStates = Nothing}
                    -- otherwise falling through to _otherReason
                    CutPointRule
                        | null filteredNexts ->
                            -- HACK. Would want to return the prior state
                            res{reason = Stuck, nextStates = Nothing}
                    _otherReason ->
                            res{state = simplifiedState, nextStates}
            pure result

        isBottom :: ExecuteState -> Bool
        isBottom ExecuteState{term}
            | KoreJson.KJBottom _ <- term.term = True
        isBottom ExecuteState{predicate = Just p}
            | KoreJson.KJBottom _ <- p.term = True
        isBottom _ = False

        runSimplify :: ExecuteState -> m ExecuteState
        runSimplify s = do
            let toSimplify = toRequestState s
            Log.logInfoNS "proxy" $ "Simplify request"
            simplResult <- kore $ Simplify SimplifyRequest{state = toSimplify, _module = mbModule}
            case simplResult of
                -- This request should not fail, as the only possible
                -- failure mode would be malformed or invalid kore
                Right (Simplify simplified) -> do
                    -- to convert back to a term/constraints form,
                    -- we run a trivial execute request (in booster)
                    -- We cannot call the booster internaliser without access to the server state
                    -- Again this should not fail.
                    let request =
                            (emptyRequest simplified.state)
                                { _module = mbModule
                                , maxDepth = Just $ Depth 0
                                }
                    Log.logInfoNS "proxy" $ "0-step execute request"
                    result <- booster $ Execute request
                    case result of
                        Right (Execute ExecuteResult{state = finalState}) -> pure finalState
                        _other -> pure s
                _other ->
                    -- TODO log error
                    pure s -- if we hit an error here, return the original

emptyRequest :: KoreJson.KoreJson -> ExecuteRequest
emptyRequest state =
    ExecuteRequest
        { state
        , maxDepth = Nothing
        , _module = Nothing
        , cutPointRules = Nothing
        , terminalRules = Nothing
        , movingAverageStepTimeout = Nothing
        , stepTimeout = Nothing
        }
