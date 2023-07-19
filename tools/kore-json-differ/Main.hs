-- | (c) FIXME
module Main where

import Control.Applicative ((<|>))
import Data.Aeson as Json
import Data.Aeson.Diff as Json
import Data.Aeson.Encode.Pretty as Json
import Data.Aeson.Types as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Network.JSONRPC
import System.Environment

import Kore.JsonRpc.Types
import Kore.Syntax.Json.Types hiding (Left, Right)

usage :: String
usage =
    unlines
        [ "Display differences between two json files containing kore-rpc data"
        , ""
        , "Usage:"
        , "       <program-name> KOREJSON1 KOREJSON2"
        , ""
        , "where KOREJSON<N> are paths to files containing a kore-rpc JSON request"
        , "a kore-rpc JSON response, or a kore term in JSON format."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        [x, y] -> diffJson x y
        other -> putStrLn $ "ERROR: program requires exactly two arguments.\n\n" <> usage

diffJson :: FilePath -> FilePath -> IO ()
diffJson korefile1 korefile2 = do
    contents1 <-
        decodeKoreRpc <$> BS.readFile korefile1
    contents2 <-
        decodeKoreRpc <$> BS.readFile korefile2

    case (contents1, contents2) of
        (_, _) | contents1 == contents2 ->
            putStrLn "The files are identical"
        (Garbled lines1, Garbled lines2) -> do
            let result = getDiff lines1 lines2
            BS.putStrLn $ 
                "Both files contain garbled json." <> renderDiff result
        (other1, other2) 
            | typeString other1 /= typeString other2 -> 
            putStrLn . unlines $
                [ "Json data in files is of different type"
                , "File " <> korefile1 <> ": " <> typeString other1
                , "File " <> korefile2 <> ": " <> typeString other2
                ]
            | otherwise -> do
                let result = getDiff (BS.lines $ Json.encodePretty other1) (BS.lines $ Json.encodePretty other2)
                BS.putStrLn $ renderDiff result

decodeKoreRpc :: BS.ByteString -> KoreRpcJson
decodeKoreRpc input =
    fromMaybe (Garbled splitInput) $
        try [rpcRequest, rpcResponse, rpcError, koreJson, unknown]
  where
    try = foldl1 (<|>)
    rpcRequest = fmap RpcRequest $ do
        req <- Json.decode @Request input
        parser <- parseParams req.getReqMethod
        Json.parseMaybe parser req.getReqParams
    rpcResponse = fmap RpcResponse $ do
        resp <- Json.decode @Response input
        foldl1
            (<|>)
            [ Execute <$> Json.parseMaybe (Json.parseJSON @ExecuteResult) resp.getResult
            , Implies <$> Json.parseMaybe (Json.parseJSON @ImpliesResult) resp.getResult
            , Simplify <$> Json.parseMaybe (Json.parseJSON @SimplifyResult) resp.getResult
            , AddModule <$> Json.parseMaybe (Json.parseJSON @()) resp.getResult
            , GetModel <$> Json.parseMaybe (Json.parseJSON @GetModelResult) resp.getResult
            ]
    rpcError =
        Json.decode @ErrorObj input >>= \err ->
            pure $ RpcError err.getErrMsg err.getErrCode err.getErrData
    koreJson =
        RpcKoreJson <$> Json.decode @KoreJson input
    unknown =
        RpcUnknown <$> Json.decode @Object input
    -- last resort: break the bytestring into lines at json-relevant 
    -- characters (ignoring quoting)
    splitInput = BS.splitWith (`elem` [':', ',', '{', '}']) input


-- | helper type enumerating all Kore-RPC json requests and responses
data KoreRpcJson
    = RpcRequest (API 'Req)
    | RpcResponse (API 'Res)
    | RpcError String Int Value
    | RpcKoreJson KoreJson
    | RpcUnknown Object
    | Garbled [BS.ByteString]
    deriving stock (Eq, Show)

instance ToJSON KoreRpcJson where
    toJSON = \case
        RpcRequest r -> toJSON r
        RpcResponse r -> toJSON r
        RpcError msg code v -> toJSON (msg, code, v)
        RpcKoreJson t -> toJSON t
        RpcUnknown v -> toJSON v
        Garbled bs -> toJSON $ map BS.unpack bs

instance ToJSON (API 'Req) where
    toJSON = \case
        Execute r -> toJSON r
        Implies r -> toJSON r
        Simplify r -> toJSON r
        AddModule r -> toJSON r
        GetModel r -> toJSON r
        Cancel -> toJSON ()        

typeString :: KoreRpcJson -> String
typeString = \case
    RpcRequest _ -> "request"
    RpcResponse _ -> "response"
    RpcError{}  -> "error response"
    RpcKoreJson _ -> "Kore term"
    RpcUnknown _ -> "unknown object"
    Garbled _ -> "garbled json"

lineDiff :: BS.ByteString -> BS.ByteString -> Diff BS.ByteString
lineDiff = getDiff `on` BS.lines

getDiff :: Eq a => [a] -> [a] -> Diff a
getDiff = undefined -- FIXME library
data Diff a -- FIXME library

renderDiff :: Show a => Diff a -> BS.ByteString
renderDiff _ = "renderDiff: IMPLEMENT ME"
