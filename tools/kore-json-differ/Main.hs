-- | (c) FIXME
module Main (main) where

import Control.Applicative ((<|>))
import Data.Aeson as Json
import Data.Aeson.Encode.Pretty as Json
import Data.Aeson.Types as Json
import Data.Algorithm.Diff as Diff
import Data.Algorithm.DiffOutput as Diff
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
        , "       <program-name> [--help | KOREJSON1 KOREJSON2]"
        , ""
        , "where KOREJSON<N> are paths to files containing a kore-rpc JSON request"
        , "a kore-rpc JSON response, a kore term in JSON format, or other JSON."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        _ | "--help" `elem` args -> putStrLn usage
        [x, y] -> diffJson x y >>= BS.putStrLn
        _other -> putStrLn $ "ERROR: program requires exactly two arguments.\n\n" <> usage

diffJson :: FilePath -> FilePath -> IO BS.ByteString
diffJson korefile1 korefile2 = do
    contents1 <-
        decodeKoreRpc <$> BS.readFile korefile1
    contents2 <-
        decodeKoreRpc <$> BS.readFile korefile2

    case (contents1, contents2) of
        (_, _)
            | contents1 == contents2 ->
                pure . BS.unwords $
                    ["Files", BS.pack korefile1, "and", BS.pack korefile2, "are identical", typeString contents1 <> "s"]
        (Garbled lines1, Garbled lines2) -> do
            let result = getGroupedDiff lines1 lines2
            pure $ "Both files contain garbled json." <> renderDiff result
        (other1, other2)
            | typeString other1 /= typeString other2 ->
                pure . BS.unlines $
                    [ "Json data in files is of different type"
                    , "  * File " <> BS.pack korefile1 <> ": " <> typeString other1
                    , "  * File " <> BS.pack korefile2 <> ": " <> typeString other2
                    ]
            | otherwise -> do
                let result = computeJsonDiff other1 other2
                pure $ renderDiff result
  where
    computeJsonDiff =
        getGroupedDiff `on` (BS.lines . Json.encodePretty' rpcJsonConfig)

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
        Json.decode @ErrorObj input
            >>= \case
                ErrorObj msg code mbData ->
                    pure $ RpcError msg code mbData
                ErrorVal{} -> fail "arbitrary json can be an ErrorVal"
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
        RpcRequest req ->
            case req of -- missing instance ToJSON (API 'Req), inlined
                Execute r -> toJSON r
                Implies r -> toJSON r
                Simplify r -> toJSON r
                AddModule r -> toJSON r
                GetModel r -> toJSON r
                Cancel -> toJSON ()
        RpcResponse r -> toJSON r
        RpcError msg code v -> toJSON (msg, code, v)
        RpcKoreJson t -> toJSON t
        RpcUnknown v -> toJSON v
        Garbled bs -> toJSON $ map BS.unpack bs

typeString :: KoreRpcJson -> BS.ByteString
typeString = \case
    RpcRequest _ -> "request"
    RpcResponse _ -> "response"
    RpcError{} -> "error response"
    RpcKoreJson _ -> "Kore term"
    RpcUnknown _ -> "unknown object"
    Garbled _ -> "garbled json"

-------------------------------------------------------------------
-- pretty diff output
-- Currently using a String-based module from the Diff package but
-- which should be rewritten to handle Text and Char8.ByteString

renderDiff :: [Diff [BS.ByteString]] -> BS.ByteString
renderDiff = BS.pack . ppDiff . map (convert (map BS.unpack))

-- Should we defined `Functor Diff`? But then again `type Diff a = PolyDiff a a`
-- and we should define `Bifunctor PolyDiff` and assimilate the `Diff` package.
convert :: (a -> b) -> Diff a -> Diff b
convert f = \case
    First a -> First $ f a
    Second b -> Second $ f b
    Both a b -> Both (f a) (f b)
