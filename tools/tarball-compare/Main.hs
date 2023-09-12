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
import Control.Monad.Trans.Except
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.FilePath

import Kore.JsonRpc.Types
import System.Environment (getArgs)
import Control.Monad (forM_)
import Control.Monad.Extra (when)

data BugReportData = BugReportData {
    requests :: Map FilePath BS.ByteString
  , responses :: Map FilePath BS.ByteString
} deriving Show

instance Semigroup BugReportData where
    (BugReportData req1 resp1) <> (BugReportData req2 resp2) = BugReportData (req1 <> req2) (resp1 <> resp2)

instance Monoid BugReportData where
    mempty = BugReportData mempty mempty


data BugReportDiff = BugReportDiff {
    booster :: BugReportData,
    koreRpc :: BugReportData
} deriving Show

instance Semigroup BugReportDiff where
    (BugReportDiff b1 k1) <> (BugReportDiff b2 k2) = BugReportDiff (b1 <> b2) (k1 <> k2)

instance Monoid BugReportDiff where
    mempty = BugReportDiff mempty mempty


main :: IO ()
main = getArgs >>= \case
    [tarFile] -> do
        contents <- Tar.checkSecurity . unpack tarFile <$> BS.readFile tarFile
        case unpackBugReports contents of
            Left err -> either print print err
            Right bugReports -> forM_ (Map.toList bugReports) $ \(file, BugReportDiff{booster, koreRpc}) -> do
                putStrLn $ "------------- " <> file <> " -------------"
                if null $ Map.keys booster.requests then
                    putStrLn $ "No Booster data... skipping..."
                    else do
                        when (Map.keys koreRpc.requests /= Map.keys booster.requests || Map.keys koreRpc.responses /= Map.keys booster.responses) $ 
                            putStrLn "Booster and kore-rpc have different requests/responses"
                        forM_ (Map.toList koreRpc.requests) $ \(koreRpcReqKey, koreRpcReqJson) -> do
                            case Map.lookup koreRpcReqKey booster.requests of
                                Nothing -> putStrLn $ "Request " <> koreRpcReqKey <> " does not exist in booster"
                                Just boosterReqJson ->
                                    when (koreRpcReqJson /= boosterReqJson) $
                                        putStrLn $ "Request " <> koreRpcReqKey <> " differs in booster"
                            case (Map.lookup koreRpcReqKey koreRpc.responses, Map.lookup koreRpcReqKey booster.responses) of
                                (Just koreResp, Just boosterResp) ->
                                    when (koreResp /= boosterResp) $
                                        putStrLn $ "Response " <> koreRpcReqKey <> " differs in booster"
                                (Just _, Nothing) -> putStrLn $ "Response " <> koreRpcReqKey <> " missing in booster"
                                (Nothing, Just _) -> putStrLn $ "Response " <> koreRpcReqKey <> " missing in kore-rpc"
                                (Nothing, Nothing) -> putStrLn $ "Response " <> koreRpcReqKey <> " missing"

    _ -> putStrLn "incorrect args"
    where
        unpack tarFile
            | ".tar" == takeExtension tarFile = Tar.read
            | ".tgz" == takeExtension tarFile = Tar.read . GZip.decompress
            | ".tar.gz" `isSuffixOf` takeExtensions tarFile = Tar.read . GZip.decompress
            | ".tar.bz2" `isSuffixOf` takeExtensions tarFile = Tar.read . BZ2.decompress
            | otherwise = Tar.read


unpackBugReports :: 
    Tar.Entries (Either Tar.FormatError Tar.FileNameError) ->
    Either (Either Tar.FormatError Tar.FileNameError) (Map FilePath BugReportDiff)
unpackBugReports = Tar.foldEntries unpackBugReportData (Right mempty) Left
    where
        unpackBugReportData ::
            Tar.Entry ->
            Either (Either Tar.FormatError Tar.FileNameError) (Map FilePath BugReportDiff) ->
            Either (Either Tar.FormatError Tar.FileNameError) (Map FilePath BugReportDiff)
        unpackBugReportData _ err@(Left _) = err
        unpackBugReportData entry acc@(Right m)
            | Tar.NormalFile bs _size <- Tar.entryContent entry
            , ".tar" `isSuffixOf` file
            , contents <- Tar.read bs = 
                case unpackBugReportDataFrom contents of
                    Left err -> Left $ Left err
                    Right bugReport -> Right $ Map.alter (insertBugReport bugReport) file m    
            | otherwise = acc
         where
            (dir, file) = splitFileName (Tar.entryPath entry)
            insertBugReport b bDiff = Just $ (\bugReportDiff -> if "booster" `isInfixOf` dir 
                    then bugReportDiff {booster = b}
                    else bugReportDiff {koreRpc = b}) $ fromMaybe mempty bDiff

{- Unpack all files inside rpc_* directories in a tarball, into maps
   of file prefixes (numbers) to requests and resp. responses.

   Assumes there is a single rpc_* directory in the tarball (fails on
   duplicate file base names).
-}
unpackBugReportDataFrom ::
    Tar.Entries err ->
    Either err BugReportData
unpackBugReportDataFrom = Tar.foldEntries unpackRpc (Right mempty) Left
  where
    unpackRpc ::
        Tar.Entry ->
        Either err BugReportData ->
        Either err BugReportData
    unpackRpc _ err@(Left _) = err
    unpackRpc entry acc@(Right BugReportData{requests, responses})
        | Tar.NormalFile bs _size <- Tar.entryContent entry
        , "rpc_" `isPrefixOf` dir
        , ".json" `isSuffixOf` file =
            let (isRequest, number, json)
                    | Just num <- stripSuffix requestSuffix file =
                        (True, num, bs)
                    | Just num <- stripSuffix responseSuffix file =
                        (False, num, bs)
                    | otherwise = error $ "Bad file in tarball: " <> show (dir </> file)
            in
                Right $ if isRequest
                    then BugReportData {requests = Map.insert number json requests, responses}
                    else BugReportData {requests, responses = Map.insert number json responses}
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
