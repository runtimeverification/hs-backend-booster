{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Simple tool to compare two tarballs for correspondence. The following
comparisons are performed until a comparison fails:

1) tarballs contain the same number of request/response files
Then, for each pair of responses to an execute request:
  2) the responses have the same number of steps
  3) the file size is the same (?)
  4) the contained json is the same (?)

The tool takes two tarballs as arguments, and program options to
determine what checks to perform (1-4 above).
-}
module Main (
    module Main,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.BZip qualified as BZ2
import Codec.Compression.GZip qualified as GZip
import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath

import Kore.JsonRpc.Types

main :: IO ()
main = undefined

{- Unpack all files inside rpc_* directories in a tarball, into maps
   of file prefixes (numbers) to requests and resp. responses.

   Assumes there is a single rpc_* directory in the tarball (fails on
   duplicate file base names).
-}
unpackRpcFrom ::
    FilePath ->
    IO (Map FilePath BS.ByteString, Map FilePath BS.ByteString)
unpackRpcFrom tarFile = do
    contents <- Tar.checkSecurity . unpack <$> BS.readFile tarFile
    Tar.foldEntries unpackRpc (pure (Map.empty, Map.empty)) throwAnyError contents
  where
    unpack
        | ".tar" == takeExtension tarFile = Tar.read
        | ".tgz" == takeExtension tarFile = Tar.read . GZip.decompress
        | ".tar.gz" `isSuffixOf` takeExtensions tarFile = Tar.read . GZip.decompress
        | ".tar.bz2" `isSuffixOf` takeExtensions tarFile = Tar.read . BZ2.decompress
        | otherwise = Tar.read

    throwAnyError :: Either Tar.FormatError Tar.FileNameError -> IO a
    throwAnyError = either throwIO throwIO

    unpackRpc ::
        Tar.Entry ->
        IO (Map FilePath BS.ByteString, Map FilePath BS.ByteString) ->
        IO (Map FilePath BS.ByteString, Map FilePath BS.ByteString)
    unpackRpc entry acc
        | Tar.NormalFile bs _size <- Tar.entryContent entry
        , "rpc_" `isPrefixOf` dir
        , ".json" `isSuffixOf` file = do
            (reqMap, respMap) <- acc
            let (isRequest, number, contents)
                    | Just num <- stripSuffix requestSuffix file =
                        (True, num, bs)
                    | Just num <- stripSuffix responseSuffix file =
                        (False, num, bs)
                    | otherwise = error $ "Bad file in tarball: " <> show (dir </> file)
            if isRequest
                then pure (Map.insert number contents reqMap, respMap)
                else pure (reqMap, Map.insert number contents respMap)
        | otherwise = acc
      where
        (dir, file) = splitFileName (Tar.entryPath entry)
        requestSuffix = "_request.json"
        responseSuffix = "_response.json"

data Interaction = Interaction
    { method :: APIMethod
    , request :: BS.ByteString
    , response :: BS.ByteString
    }
    deriving (Eq, Ord, Show)

mkInteractions ::
    Map FilePath BS.ByteString ->
    Map FilePath BS.ByteString ->
    Except String (Map FilePath Interaction)
mkInteractions requests responses
    | not (Map.null surplusReqs) =
        throwE $ "Surplus requests: " <> show (Map.keys surplusReqs)
    | not (Map.null surplusResps) =
        throwE $ "Surplus responses: " <> show (Map.keys surplusResps)
    | otherwise =
        pure $ Map.intersectionWith mkInteraction requests responses
  where
    surplusReqs = Map.difference requests responses
    surplusResps = Map.difference responses requests

mkInteraction :: BS.ByteString -> BS.ByteString -> Interaction
mkInteraction request response = Interaction{method, request, response}
  where
    method =
        fromMaybe (error "Unknown RPC method in request file") $
        either error getMethod requestData

    requestData = Json.eitherDecode request :: Either String Json.Object

    getMethod :: Json.Object -> Maybe APIMethod
    getMethod obj = Json.lookup "method" obj >>= apiMethod

apiMethod :: Json.Value -> Maybe APIMethod
apiMethod (Json.String "execute") = Just ExecuteM
apiMethod (Json.String "implies") = Just ImpliesM
apiMethod (Json.String "simplify") = Just SimplifyM
apiMethod (Json.String "add-module") = Just AddModuleM
apiMethod (Json.String "get-model") = Just GetModelM
apiMethod _other = Nothing
