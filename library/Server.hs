{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Server (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Control.Monad.Logger (LogLevel (..))
import Data.List (intercalate, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack, unpack)
import Options.Applicative
import Text.Casing
import Text.Read

import Booster.JsonRpc (runServer)
import Booster.LLVM.Internal (mkAPI, withDLib)
import Booster.Syntax.ParsedKore (loadDefinition)
import Booster.Trace
import Booster.VersionInfo (VersionInfo (..), versionInfo)

main :: IO ()
main = do
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
    definitionMap <-
        loadDefinition definitionFile
            >>= evaluate . force . either (error . show) id
    -- ensure the (default) main module is present in the definition
    when (isNothing $ Map.lookup mainModuleName definitionMap) $
        error $
            "Module " <> unpack mainModuleName <> " does not exist in the given definition."
    putStrLn "Starting RPC server"
    case llvmLibraryFile of
        Nothing -> runServer port definitionMap mainModuleName Nothing (adjustLogLevels logLevels)
        Just fp -> withDLib fp $ \dl -> do
            api <- mkAPI dl
            runServer port definitionMap mainModuleName (Just api) (adjustLogLevels logLevels)
  where
    clParser =
        info
            (clOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    , llvmLibraryFile :: Maybe FilePath
    , port :: Int
    , logLevels :: [LogLevel]
    , eventlogEnabledUserEvents :: [CustomUserEventType]
    }
    deriving (Show)

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
