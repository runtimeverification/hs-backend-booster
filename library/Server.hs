{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Server (main) where

import Data.Text (Text)
import Kore.JsonRpc (TODOInternalizedModule (..), runServer)
import Options.Applicative (InfoMod, Parser, argument, auto, execParser, fullDesc, header, help, info, long, metavar, option, short, str, switch)

main :: IO ()
main = do
    options <- execParser (info clOptionsParser parserInfoModifiers)
    let CLOptions{port} = options
    runServer port TODOInternalizedModule
    return ()

data CLOptions = CLOptions
    { askVersion :: !Bool
    , askHelp :: !Bool
    , definitionFile :: !FilePath
    , mainModuleName :: !Text
    , port :: !Int
    }

parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "kore-rpc - a JSON RPC server for symbolically executing Kore definitions"

clOptionsParser :: Parser CLOptions
clOptionsParser =
    CLOptions
        <$> switch
            ( long "version"
                <> short 'v'
                <> help "Print version info."
            )
        <*> switch
            ( long "help"
                <> short 'h'
                <> help "Print help info."
            )
        <*> argument
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
