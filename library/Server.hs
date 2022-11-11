{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Server (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Options.Applicative

import Kore.JsonRpc (runServer)
import Kore.Syntax.ParsedKore (loadDefinition)
import Kore.VersionInfo (VersionInfo (..), versionInfo)

main :: IO ()
main = do
    options <- execParser clParser
    let CLOptions{definitionFile, mainModuleName, port} = options
    putStrLn $
        "Loading definition from "
            <> definitionFile
            <> ", main module "
            <> show mainModuleName
    internalModule <-
        either (error . show) id
            <$> loadDefinition mainModuleName definitionFile
    putStrLn "Starting RPC server"
    runServer port internalModule
  where
    clParser =
        info
            (clOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

data CLOptions = CLOptions
    { definitionFile :: !FilePath
    , mainModuleName :: !Text
    , port :: !Int
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
                <> help "Port for the RPC server to bind to"
            )

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
