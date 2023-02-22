{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Catch(bracket)
import Control.Monad.Logger (LogLevel (..))
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Options.Applicative

import Control.Monad (forM_, void)
import Booster.LLVM.Internal (mkAPI, withDLib)
import Booster.Syntax.ParsedKore (loadDefinition)
import Booster.Trace
import Booster.VersionInfo (VersionInfo (..), versionInfo)

import Text.Casing
import Text.Read
import Options.SMT (KoreSolverOptions (..), parseKoreSolverOptions)
import qualified GlobalMain
import qualified Control.Concurrent.MVar as MVar
import Kore.JsonRpc (ServerState(..))
import qualified SMT
import qualified Data.Map as Map
import qualified Kore.Log as Log
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.Syntax.Definition (SentenceAxiom, ModuleName (ModuleName))
import Kore.Internal.TermLike (TermLike, VariableName)
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Kore.Rewrite.SMT.Lemma (declareSMTLemmas)
import Proxy qualified
import Kore.Log (withLogger, defaultKoreLogOptions, swappableLogger, logType, KoreLogType (..), ExeName (..))
import Kore.BugReport (withBugReport, BugReportOption (..), ExitCode (ExitSuccess))
import Control.Concurrent.MVar (newMVar)
import System.Clock (
    Clock (..),
    getTime,
 )


main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <- execParser clParser
    let CLOptions{definitionFile, mainModuleName, port, logLevels, llvmLibraryFile, eventlogEnabledUserEvents} = options

    forM_ eventlogEnabledUserEvents $ \t -> do
        putStrLn $ "Tracing " <> show t
        enableCustomUserEvent t
    putStrLn $
        "Loading definition from "
            <> definitionFile
            <> ", main module "
            <> show mainModuleName
    definition <-
        loadDefinition mainModuleName definitionFile
            >>= evaluate . force . either (error . show) id

    void $ withBugReport (ExeName "hs-booster-proxy") BugReportOnError $ \reportDirectory ->
        withLogger reportDirectory (defaultKoreLogOptions (ExeName "hs-booster-proxy") startTime){logType = LogFileText "kore.log"} $ \actualLogAction -> do
            mvarLogAction <- newMVar actualLogAction
            let logAction = swappableLogger mvarLogAction


            kore <- mkKoreServer Log.LoggerEnv{logAction} options

            withMDLib llvmLibraryFile $ \mdl -> do
                mLlvmLibrary <- maybe (pure Nothing) (fmap Just . mkAPI) mdl

                let (logLevel, customLevels) = adjustLogLevels logLevels
                    booster = Proxy.BoosterServer{
                        definition
                        , mLlvmLibrary
                        , logLevel
                        , customLevels
                        }

                putStrLn "Starting RPC server"

                Proxy.runServer port kore booster
                pure ExitSuccess
  where
    clParser =
        info
            (clOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

    withMDLib Nothing f = f Nothing
    withMDLib (Just fp) f = withDLib fp $ \dl -> f (Just dl)

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    , llvmLibraryFile :: Maybe FilePath
    , port :: Int
    , logLevels :: [LogLevel]
    , eventlogEnabledUserEvents :: [CustomUserEventType]
    , koreSolverOptions :: !KoreSolverOptions
    }
    

parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "Haskell Backend Booster - a JSON RPC server for quick symbolic execution of Kore definitions"

versionInfoParser :: Parser (a -> a)
versionInfoParser =
    infoOption
        versionInfoStr
        ( short 'v'
            <> long "version"
            <> help "Print version info."
        )

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
        <*> optional
            ( strOption
                ( metavar "LLVM_BACKEND_LIBRARY"
                    <> long "llvm-backend-library"
                    <> help "Path to the .so/.dylib shared LLVM backend library"
                )
            )
        <*> option
            auto
            ( metavar "SERVER_PORT"
                <> long "server-port"
                <> value 31337
                <> help "Port for the RPC server to bind to"
                <> showDefault
            )
        <*> many
            ( option
                (eitherReader readLogLevel)
                ( metavar "LEVEL"
                    <> long "log-level"
                    <> short 'l'
                    <> help
                        ( "Log level: debug, info (default), warn, error, \
                          \or a custom level from "
                            <> show allowedLogLevels
                        )
                )
            )
        <*> many
            ( option
                (eitherReader readEventLogTracing)
                ( metavar "TRACE"
                    <> long "trace"
                    <> short 't'
                    <> help
                        ( "Eventlog tracing options: "
                            <> intercalate ", " [toKebab $ fromHumps $ show t | t <- [minBound .. maxBound] :: [CustomUserEventType]]
                        )
                )
            )
        <*> parseKoreSolverOptions
  where
    readLogLevel :: String -> Either String LogLevel
    readLogLevel = \case
        "debug" -> Right LevelDebug
        "info" -> Right LevelInfo
        "warn" -> Right LevelWarn
        "error" -> Right LevelError
        other
            | other `elem` allowedLogLevels -> Right (LevelOther $ pack other)
            | otherwise -> Left $ other <> ": Unsupported log level"

    readEventLogTracing :: String -> Either String CustomUserEventType
    readEventLogTracing =
        (\s -> maybe (Left $ s <> " not supported in eventlog tracing") Right $ readMaybe s) . toPascal . fromKebab

-- custom log levels that can be selected
allowedLogLevels :: [String]
allowedLogLevels = ["Rewrite"]

-- Partition provided log levels into standard and custom ones, and
-- select the lowest standard level. Default to 'LevelInfo' if no
-- standard log level was given.
adjustLogLevels :: [LogLevel] -> (LogLevel, [LogLevel])
adjustLogLevels ls = (standardLevel, customLevels)
  where
    (stds, customLevels) = partition (<= LevelError) ls
    standardLevel = if null stds then LevelInfo else minimum stds

versionInfoStr :: String
versionInfoStr =
    unlines
        [ "hs-backend-booster version:"
        , "  revision:\t" <> gitHash <> if gitDirty then " (dirty)" else ""
        , "  branch:\t" <> fromMaybe "<unknown>" gitBranch
        , "  last commit:\t" <> gitCommitDate
        ]
  where
    VersionInfo{gitHash, gitDirty, gitBranch, gitCommitDate} = $versionInfo






mkKoreServer :: Log.LoggerEnv IO -> CLOptions -> IO Proxy.KoreServer
mkKoreServer loggerEnv@Log.LoggerEnv{logAction} CLOptions { definitionFile, mainModuleName, koreSolverOptions} = 
    flip Log.runLoggerT logAction $ do
        sd@GlobalMain.SerializedDefinition{internedTextCache} <-
            GlobalMain.deserializeDefinition
                koreSolverOptions
                definitionFile
                (ModuleName mainModuleName)
        liftIO $ writeIORef globalInternedTextCache internedTextCache

        loadedDefinition <- GlobalMain.loadDefinitions [definitionFile]
        serverState <-
            liftIO $
                MVar.newMVar
                    ServerState
                        { serializedModules = Map.singleton (ModuleName mainModuleName) sd
                        , loadedDefinition
                        }

        pure $ Proxy.KoreServer {
            serverState,
            mainModule = mainModuleName,
            runSMT,
            loggerEnv
        }
    
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
