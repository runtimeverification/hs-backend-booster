{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Server (main) where

import Control.Monad.Logger (LogLevel (..))
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Options.Applicative
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import Kore.JsonRpc (runServer)
import Kore.Syntax.ParsedKore (loadDefinition)
import Kore.VersionInfo (VersionInfo (..), versionInfo)

main :: IO ()
main = do
    options <- execParser clParser
    let CLOptions{definitionFile, mainModuleName, port, logLevels} = options
    putStrLn $
        "Loading definition from "
            <> definitionFile
            <> ", main module "
            <> show mainModuleName
    internalModule <-
            force
            <$> either (error . show) id
            <$> loadDefinition mainModuleName definitionFile
            >>= evaluate
    putStrLn "Starting RPC server"
    runServer port internalModule (adjustLogLevels logLevels)
  where
    clParser =
        info
            (clOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    , port :: Int
    , logLevels :: [LogLevel]
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
        <$> argument
            str
            ( metavar "DEFINITION_FILE"
                <> help "Kore definition file to verify and use for execution"
            )
        <*> option
            str
            ( metavar "MODULE"
                <> long "module"
                <> help "The name of the main module in the Kore definition."
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
    standardLevel = minimum (LevelInfo : stds)

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
