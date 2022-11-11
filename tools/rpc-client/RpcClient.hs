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

import Control.Monad
import Data.Aeson qualified as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Options.Applicative
import System.Exit

import Kore.JsonRpc.Base qualified as Rpc
import Kore.Syntax.Json.Base qualified as Syntax

import Debug.Trace

main :: IO ()
main = do
    error "implement me!"
    Options{host, port, action, optionFile, options, expectFile} <- execParser parseOptions
    request <-
        trace "Preparing request data" $
        prepareRequestData action optionFile options
    result <- runTCPClient host (show port) $ \s -> do
        trace "Sending request..." $
            sendAll s request
        response <- recv s 8192
        trace "Response received." $
            pure response
    maybe BS.putStrLn compareToExpectation expectFile $ result

data Options =
    Options
    { host :: String
    , port :: Int
    , action :: Action -- what to do
    , optionFile :: Maybe FilePath -- file with options (different for each endpoint
    , options :: [(String, String)] -- verbatim options (name, value) to add to json
    , expectFile :: Maybe FilePath -- whether to diff to an expectation file or output
    }

-- | Defines what to do. Either one of the endpoints, or raw data
data Action
    = Exec FilePath
    | Simpl FilePath
    | Check FilePath
    | SendRaw FilePath

parseOptions :: ParserInfo Options
parseOptions = undefined

prepareRequestData :: Action -> Maybe FilePath -> [(String, String)] -> IO BS.ByteString
prepareRequestData (SendRaw file) _ _ =
    BS.readFile file
prepareRequestData (Exec file) opts mbOptFile = do
    undefined

compareToExpectation :: FilePath -> BS.ByteString -> IO ()
compareToExpectation expectFile output = do
    expected <- BS.readFile expectFile
    -- TODO https://hackage.haskell.org/package/Diff
    when (output /= expected) $ do
        BS.putStrLn output
        BS.putStrLn "Not the same, sorry."
        exitWith $ ExitFailure 1
