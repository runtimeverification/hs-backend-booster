{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

A very simple RPC client to use for testing HS backend RPC servers

* can send file content verbatim
* can construct 'KorePattern' data

* Results can be compared to expected output files, displaying a diff
  if the response is not the expected one.
-}
module RpcClient (
    main,
) where

import Data.Aeson qualified as Json
import Data.Conduit.Network (serverSettings, clientSettings)
import Data.Text.IO qualified as Text
import Network.JSONRPC
import Options.Applicative

import Kore.JsonRpc.Base qualified as Rpc
import Kore.Syntax.Json.Base qualified as Syntax

main :: IO ()
main = do
    error "implement me!"
    Options{host, port} <- execParser parseOptions
    result <-
        jsonrpcTCPClient
            V2
            True {- ignore-incoming -}
            (clientSettings host port) $
            action  -- the action
  where
    action = undefined

data Options =
    Options
    { host :: Text
    , port :: Int
    , raw :: Bool -- whether to send the file data verbatim or use the below
    , verb :: Action -- what to do
    , optionFile :: Maybe FilePath -- file with options (different for each endpoint
    , options :: [(Text, Text)] -- verbatim options (name, value) to add to json
    , expectFile :: Maybe FilePath -- whether to diff to an expectation file or output
    }

data Action = Exec FilePath |  Simpl FilePath | Check FilePath -- the endpoints

parseOptions :: Parser Options
parseOptions = undefined
