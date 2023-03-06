{- |
Module      : Booster.JsonRpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.JsonRpc (
    module Booster.JsonRpc,
    rpcJsonConfig,
) where

import Control.Exception (ErrorCall (..), SomeException)
import Control.Monad.Catch (MonadCatch, MonadMask, handle)
import Control.Monad.Logger.CallStack (LogLevel (LevelError), MonadLoggerIO)
import Control.Monad.Logger.CallStack qualified as Log
import Control.Monad.Trans.Except (runExcept)
import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Value (..))
import Data.Conduit.Network (serverSettings)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Numeric.Natural

import Booster.Definition.Base (KoreDefinition (..))
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base (Pattern)
import Booster.Pattern.Rewrite (RewriteResult (..), performRewrite)
import Booster.Syntax.Json (KoreJson (..), addHeader)
import Booster.Syntax.Json.Externalise (externalisePattern)
import Booster.Syntax.Json.Internalise (PatternError, internalisePattern)
import Control.Monad.Logger (logInfoN)
import Kore.JsonRpc.Server
import Kore.JsonRpc.Types

respond ::
    forall m.
    MonadMask m =>
    MonadLoggerIO m =>
    KoreDefinition ->
    Maybe LLVM.API ->
    Respond (API 'Req) m (API 'Res)
respond def@KoreDefinition{} mLlvmLibrary =
    catchingServerErrors . \case
        Execute req
            | isJust req._module -> unsupportedField "module"
            | isJust req.stepTimeout -> unsupportedField "step-timeout"
            | isJust req.movingAverageStepTimeout -> unsupportedField "moving-average-step-timeout"
        Execute req -> do
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

        -- this case is only reachable if the cancel appeared as part of a batch request
        Cancel -> pure $ Left $ ErrorObj "Cancel request unsupported in batch mode" (-32001) Null
        -- using "Method does not exist" error code

        _ -> pure $ Left $ ErrorObj "Not implemented" (-32601) Null
  where
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
        ExecuteState{term = addHeader t, predicate = fmap addHeader p, substitution = Nothing}
      where
        (t, p) = externalisePattern pat

    reportPatternError :: PatternError -> ErrorObj
    reportPatternError pErr =
        ErrorObj "Could not verify KORE pattern" (-32002) $ toJSON pErr

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

runServer :: Int -> KoreDefinition -> Maybe LLVM.API -> (LogLevel, [LogLevel]) -> IO ()
runServer port internalizedModule mLlvmLibrary (logLevel, customLevels) =
    do
        Log.runStderrLoggingT . Log.filterLogger levelFilter
        $ jsonRpcServer
            rpcJsonConfig
            srvSettings
            (const $ respond internalizedModule mLlvmLibrary)
            [JsonRpcHandler $ \(err :: SomeException) -> logInfoN (Text.pack $ show err) >> pure (ErrorObj "Server error: crashed" (-32032) $ toJSON $ show err)]
  where
    levelFilter _source lvl =
        lvl `elem` customLevels || lvl >= logLevel && lvl <= LevelError

    srvSettings = serverSettings port "*"
