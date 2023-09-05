{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Control.Exception
    ( ErrorCall, catch )
import Control.Monad (forM_, void)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Monad.Logger (
    LogLevel (..),
    LoggingT (runLoggingT),
    MonadLoggerIO (askLoggerIO),
    ToLogStr (toLogStr),
    defaultLoc,
 )
import Control.Monad.Logger qualified as Logger
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text

import Options.Applicative
import System.Clock (
    Clock (..),
    getTime,
 )
import System.Exit

-- import Booster.CLOptions
import GlobalMain qualified
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.BugReport (BugReportOption (..), withBugReport)
import Kore.Exec qualified as Exec
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.Log (
    ExeName (..),
    KoreLogType (LogSomeAction),
    LogAction (LogAction),
    TimestampsSwitch (TimestampsDisable),
    defaultKoreLogOptions,
    swappableLogger,
    withLogger,
 )
import Kore.Log qualified
import Kore.Log qualified as Log
import Kore.Rewrite.SMT.Lemma (declareSMTLemmas)
import Kore.Syntax.Definition (ModuleName (ModuleName), SentenceAxiom)
import Options.SMT (KoreSolverOptions (..), parseKoreSolverOptions)
import SMT qualified
import Booster.Syntax.ParsedKore qualified as ParsedKore

import Booster.Prettyprinter qualified as Pretty
import Prettyprinter qualified as Pretty
import Booster.Definition.Base (KoreDefinition(..), RewriteRule(..))
import Kore.Validate.PatternVerifier qualified as PatternVerifier
import Kore.Internal.Pattern qualified as Pattern
import Kore.Syntax.Json qualified as PatternJson
import qualified Kore.Rewrite.SMT.Evaluator as SMT.Evaluator
import qualified Kore.Simplify.Pattern as Pattern
import Kore.Log.DecidePredicateUnknown (DecidePredicateUnknown, srcLoc)
import Kore.Simplify.API (evalSimplifier)
import Kore.Rewrite.Step (mkRewritingPattern)
import Booster.Syntax.Json.Externalise (externaliseTerm, externaliseSort, externalisePredicate)
import Kore.Syntax.Json.Types qualified as Json
import Booster.Pattern.Util (sortOfTerm)
import qualified Kore.Builtin as Builtin
import Prelude.Kore ((&))
import Kore.TopBottom (isTop)
import Booster.Definition.Attributes.Base (ComputedAxiomAttributes(..), AxiomAttributes (..))

main :: IO ()
main = do
    startTime <- getTime Monotonic
    (clOPts, koreSolverOptions) <- execParser clParser


    Logger.runStderrLoggingT $ do

        monadLogger <- askLoggerIO

        let logLevel = LevelInfo
            coLogLevel = fromMaybe Log.Info $ toSeverity logLevel
            koreLogOptions =
                (defaultKoreLogOptions (ExeName "") startTime)
                    { Log.logLevel = coLogLevel
                    , Log.timestampsSwitch = TimestampsDisable
                    , Log.logType = LogSomeAction $ LogAction $ \txt -> liftIO $ monadLogger defaultLoc "kore" logLevel $ toLogStr txt
                    }
            -- srvSettings = serverSettings port "*"

        liftIO $ void $ withBugReport (ExeName "preserves-definedness") BugReportOnError $ \reportDirectory ->
            withLogger reportDirectory koreLogOptions $ \actualLogAction -> do
                mvarLogAction <- newMVar actualLogAction
                let logAction = swappableLogger mvarLogAction

                (serializedModule@Exec.SerializedModule{ sortGraph
                        , overloadGraph
                        , metadataTools
                        , verifiedModule
                        , equations
                        }, runSMT) <- mkKoreServer Log.LoggerEnv{logAction} clOPts koreSolverOptions
                runLoggingT (Logger.logInfoNS "proxy" "Starting RPC server") monadLogger


                rawDef <- liftIO (Text.readFile clOPts.definitionFile)
                let minternalDef = do
                        parsedDef <- ParsedKore.parseKoreDefinition clOPts.definitionFile rawDef
                        either ( Left . Pretty.renderDefault . Pretty.pretty) Right $ ParsedKore.internalise Nothing parsedDef
                case minternalDef of
                    Left err -> putStrLn err >> pure (ExitFailure 1)

                    Right KoreDefinition{rewriteTheory, functionEquations} -> do
                        forM_ (concat $ concatMap Map.elems $ Map.elems functionEquations) $ \RewriteRule{attributes, computedAttributes=ComputedAxiomAttributes{notPreservesDefinednessReasons}, lhs, rhs, requires, ensures} -> do

                            if null notPreservesDefinednessReasons
                                then pure ()
                                else do
                                    print $ "------------------------------------------------------ " <> show attributes.location <> " ------------------------------------------------------"
                                    -- #Ceil(LHS #And REQ) #Implies #Ceil(RHS #And ENS)
                                    let sort = externaliseSort $ sortOfTerm lhs
                                        ceilTerm =
                                            Json.KJImplies sort
                                                (Json.KJCeil sort sort (
                                                    foldr (Json.KJAnd sort)
                                                        (externaliseTerm lhs)
                                                        [externalisePredicate sort r | r <- requires]
                                                    ))
                                                (Json.KJCeil sort sort (
                                                    foldr (Json.KJAnd sort)
                                                        (externaliseTerm rhs)
                                                        [externalisePredicate sort e | e <- ensures]
                                                    ))
                                    case verifyIn serializedModule ceilTerm of
                                        Left err -> print err
                                        Right ceilTermVerified -> do
                                            let patt =
                                                    mkRewritingPattern $ Pattern.parsePatternFromTermLike ceilTermVerified
                                                -- sort = TermLike.termLikeSort ceilTermVerified

                                            mresult <-
                                                liftIO
                                                    . flip catch (\(_ :: ErrorCall) -> pure Nothing)
                                                    . flip catch (\(_ :: DecidePredicateUnknown) -> pure Nothing)
                                                    . (Just <$>)
                                                    . runSMT
                                                    . evalSimplifier
                                                        verifiedModule
                                                        sortGraph
                                                        overloadGraph
                                                        metadataTools
                                                        equations
                                                    $ SMT.Evaluator.filterMultiOr $srcLoc =<< Pattern.simplify patt

                                            case mresult of
                                                Nothing -> print "could not decide Ceil!"
                                                Just result ->

                                                    if isTop result
                                                        then print $ "!!!!!!!!!!! rule preserves definedness"
                                                        else print $ Pretty.pretty result



                        pure ExitSuccess



  where
    clParser =
        info
            (((,) <$> clOptionsParser <*> parseKoreSolverOptions) <**> helper)
            parserInfoModifiers

    verifyIn m json =
        PatternVerifier.runPatternVerifier (verifierContext m) $
            PatternVerifier.verifyStandalonePattern Nothing $
                PatternJson.toParsedPattern json

    verifierContext Exec.SerializedModule{verifiedModule} =
        PatternVerifier.verifiedModuleContext verifiedModule
            & PatternVerifier.withBuiltinVerifiers Builtin.koreVerifiers
            & (\context -> context{PatternVerifier.isRpcRequest = True})


toSeverity :: LogLevel -> Maybe Log.Severity
toSeverity LevelDebug = Just Log.Debug
toSeverity LevelInfo = Just Log.Info
toSeverity LevelWarn = Just Log.Warning
toSeverity LevelError = Just Log.Error
toSeverity LevelOther{} = Nothing

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    }
    deriving (Show)

clOptionsParser :: Parser CLOptions
clOptionsParser =
    CLOptions
        <$> strArgument
            ( metavar "DEFINITION_FILE"
                <> help "Kore definition file to verify and use for execution"
            )
        <*> strOption
            ( metavar "MODULE"
                <> long "module"
                <> help "The name of the main module in the Kore definition."
            )
parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "preserves-definedness - a kompiler pass for computing definedness conditiosn for kore definition rules"

mkKoreServer :: Log.LoggerEnv IO -> CLOptions -> KoreSolverOptions -> IO (Exec.SerializedModule, SMT.SMT a -> IO a)
mkKoreServer Log.LoggerEnv{logAction} CLOptions{definitionFile, mainModuleName} koreSolverOptions =
    flip Log.runLoggerT logAction $ do
        sd@GlobalMain.SerializedDefinition{serializedModule, lemmas, internedTextCache} <-
            GlobalMain.deserializeDefinition
                koreSolverOptions
                definitionFile
                (ModuleName mainModuleName)
        liftIO $ writeIORef globalInternedTextCache internedTextCache

        -- loadedDefinition <- GlobalMain.loadDefinitions [definitionFile]
        pure
            (serializedModule,

            runSMT (Exec.metadataTools serializedModule) lemmas)

  where
    KoreSolverOptions{timeOut, rLimit, resetInterval, prelude} = koreSolverOptions
    smtConfig =
        SMT.defaultConfig
            { SMT.timeOut = timeOut
            , SMT.rLimit = rLimit
            , SMT.resetInterval = resetInterval
            , SMT.prelude = prelude
            }

    -- SMT solver with user declared lemmas
    runSMT ::
        forall a.
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        SMT.SMT a ->
        IO a
    runSMT metadataTools lemmas m =
        flip Log.runLoggerT logAction $
            bracket (SMT.newSolver smtConfig) SMT.stopSolver $ \refSolverHandle -> do
                let userInit = SMT.runWithSolver $ declareSMTLemmas metadataTools lemmas
                    solverSetup =
                        SMT.SolverSetup
                            { userInit
                            , refSolverHandle
                            , config = smtConfig
                            }
                SMT.initSolver solverSetup
                SMT.runWithSolver m solverSetup



