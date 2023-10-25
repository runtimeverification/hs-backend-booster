{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Simple tool to compare two tarballs for correspondence. The following
comparisons are performed until a comparison fails:

1) tarballs contain the same number of request/response files
Then, for each pair of responses to an execute request:
  . the file size is the same (?)
  . the responses have the same number of steps (depth) if the request
    is an execute request
  . the contained json is the same (?)

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
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans.Writer
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Directory
import System.Directory.Extra
import System.Environment (getArgs)
import System.FilePath

import Booster.JsonRpc.Utils
import Kore.JsonRpc.Types

data BugReportData = BugReportData
    { requests :: Map FilePath BS.ByteString
    , responses :: Map FilePath BS.ByteString
    , definition :: BS.ByteString
    }
    deriving (Show)

emptyBugReport :: BugReportData
emptyBugReport = BugReportData mempty mempty "INVALID"

data BugReportDiff = BugReportDiff
    { booster :: BugReportData
    , koreRpc :: BugReportData
    }
    deriving (Show)

emptyDiff :: BugReportDiff
emptyDiff = BugReportDiff emptyBugReport emptyBugReport

main :: IO ()
main =
    getArgs >>= \case
        [tarFile] -> do
            contents <- readToTar tarFile
            case unpackBugReports contents of
                Left err -> either print print err
                Right bugReports ->
                    forM_ (Map.toList bugReports) $
                        mapM_ BS.putStrLn . uncurry checkDiff
        [tar1, tar2] -> do
            let dataFrom =
                    fmap (either (error . either show show) id . unpackBugReportDataFrom)
                        . readToTar
            bugReportDiff <-
                BugReportDiff
                    <$> dataFrom tar1
                    <*> dataFrom tar2
            mapM_ BS.putStrLn $ checkDiff (tar1 <> "<->" <> tar2) bugReportDiff
        _ -> putStrLn "incorrect args"
  where
    readToTar :: FilePath -> IO (Tar.Entries (Either Tar.FormatError Tar.FileNameError))
    readToTar file
        | ".tar" == takeExtension file =
            Tar.checkSecurity . Tar.read <$> BS.readFile file
        | ".tgz" == takeExtension file || ".tar.gz" `isSuffixOf` takeExtensions file =
            Tar.checkSecurity . Tar.read . GZip.decompress <$> BS.readFile file
        | ".tar.bz2" `isSuffixOf` takeExtensions file =
            Tar.checkSecurity . Tar.read . BZ2.decompress <$> BS.readFile file
        | otherwise = do
            isDir <- doesDirectoryExist file
            if isDir
                then withCurrentDirectory file $ do
                    -- create a Tar.Entries structure from the rpc_*
                    -- directories within the directory (ignore all other
                    -- files and subdirectories)
                    subdirs <-
                        filter (dirPrefix `isPrefixOf`) . map takeFileName <$> listDirectories "."
                    let hasCorrectSuffix f =
                            requestSuffix `isSuffixOf` f || responseSuffix `isSuffixOf` f
                    files <-
                        filter hasCorrectSuffix . concat <$> mapM listFiles subdirs
                    defFile <-
                        fromMaybe (error $ "No definition found in " <> file)
                            . find (== "./definition.kore")
                            <$> listFiles "."

                    entries <- Tar.pack "." $ defFile : files
                    -- need to force the tar entries, withCurrentDirectory is not retained
                    mapM_ (`seq` pure ()) entries
                    pure $ foldr Tar.Next Tar.Done entries
                else -- if a differently-named file was given. try to read a tarball
                    Tar.checkSecurity . Tar.read <$> BS.readFile file

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
        insertBugReport b bDiff =
            Just
                $ ( \bugReportDiff ->
                        if "booster" `isInfixOf` dir
                            then bugReportDiff{booster = b}
                            else bugReportDiff{koreRpc = b}
                  )
                $ fromMaybe emptyDiff bDiff

{- Unpack all files inside rpc_* directories in a tarball, into maps
   of file prefixes (dir.name and number) to requests and resp. responses.

   There may be multiple rpc_* directories in a single tarball, therefore
   the map keys have to contain the directory name.
-}
unpackBugReportDataFrom ::
    Tar.Entries err ->
    Either err BugReportData
unpackBugReportDataFrom = Tar.foldEntries unpackRpc (Right emptyBugReport) Left
  where
    unpackRpc ::
        Tar.Entry ->
        Either err BugReportData ->
        Either err BugReportData
    unpackRpc _ err@(Left _) = err
    unpackRpc entry acc@(Right bugReportData)
        | Tar.NormalFile bs _size <- Tar.entryContent entry
        , Just dir <- stripPrefix dirPrefix rpcDir
        , ".json" `isSuffixOf` file =
            let (isRequest, number, json)
                    | Just num <- stripSuffix requestSuffix file =
                        (True, num, bs)
                    | Just num <- stripSuffix responseSuffix file =
                        (False, num, bs)
                    | otherwise = error $ "Bad file in tarball: " <> show (rpcDir </> file)
             in Right $
                    if isRequest
                        then bugReportData{requests = Map.insert (dir <> number) json bugReportData.requests}
                        else bugReportData{responses = Map.insert (dir <> number) json bugReportData.responses}
        | Tar.NormalFile bs _size <- Tar.entryContent entry
        , rpcDir == "./" -- dir output of splitFileName for names without path
        , file == "definition.kore" =
            Right bugReportData{definition = bs}
        | otherwise = acc
      where
        (rpcDir, file) = splitFileName (Tar.entryPath entry)

dirPrefix, requestSuffix, responseSuffix :: FilePath
dirPrefix = "rpc_"
requestSuffix = "_request.json"
responseSuffix = "_response.json"

checkDiff :: FilePath -> BugReportDiff -> [BS.ByteString]
checkDiff name BugReportDiff{booster, koreRpc} =
    "------------- " <> BS.pack name <> " -------------"
        : if null $ Map.keys booster.requests
            then ["No Booster data... skipping..."]
            else execWriter $ do
                when (booster.definition /= koreRpc.definition) $ do
                    msg $
                        "Definitions in bug reports differ "
                            <> compareSizes booster.definition koreRpc.definition
                when
                    ( Map.keys koreRpc.requests /= Map.keys booster.requests
                        || Map.keys koreRpc.responses /= Map.keys booster.responses
                    )
                    $ msg "Booster and kore-rpc have different requests/responses"
                forM (Map.toList koreRpc.requests) $ \(koreRpcReqKey, koreRpcReqJson) -> do
                    let keyBS = BS.pack koreRpcReqKey
                    case Map.lookup koreRpcReqKey booster.requests of
                        Nothing ->
                            msg $ "Request " <> keyBS <> " does not exist in booster"
                        Just boosterReqJson -> do
                            let koreTp = requestType koreRpcReqJson
                                boosTp = requestType boosterReqJson
                            when (koreTp /= boosTp) $
                                strMsg $
                                    "Requests have different type: " <> show (boosTp, koreTp)
                            compareJson
                                "Requests"
                                keyBS
                                koreRpcReqJson
                                boosterReqJson
                    case (Map.lookup koreRpcReqKey koreRpc.responses, Map.lookup koreRpcReqKey booster.responses) of
                        (Just koreResp, Just boosterResp) -> do
                            compareJson
                                "Responses"
                                keyBS
                                koreResp
                                boosterResp
                            let koreDepth = responseDepth koreResp
                                boosDepth = responseDepth boosterResp
                            when (koreDepth /= boosDepth) $
                                strMsg $
                                    "Execution depth differs: "
                                        <> show boosDepth
                                        <> " vs "
                                        <> show koreDepth
                        (Just _, Nothing) ->
                            msg $ "Response " <> keyBS <> " missing in booster"
                        (Nothing, Just _) ->
                            msg $ "Response " <> keyBS <> " missing in kore-rpc"
                        (Nothing, Nothing) ->
                            msg $ "Response " <> keyBS <> " missing"
  where
    msg = tell . (: [])
    strMsg = msg . BS.pack

    compareJson ::
        BS.ByteString ->
        BS.ByteString ->
        BS.ByteString ->
        BS.ByteString ->
        Writer [BS.ByteString] ()
    compareJson prefix key koreJson boosterJson =
        when (koreJson /= boosterJson) $
            msg $
                BS.unwords
                    [prefix, "for", key, "are different", compareSizes boosterJson koreJson]

    compareSizes bsBooster bsKore =
        case compare (BS.length bsBooster) (BS.length bsKore) of
            LT -> "(kore bigger)"
            EQ -> "(same size)"
            GT -> "(booster bigger)"

    requestType :: BS.ByteString -> KoreRpcType
    requestType = rpcTypeOf . decodeKoreRpc

    responseDepth :: BS.ByteString -> Maybe Depth
    responseDepth json =
        case decodeKoreRpc json of
            RpcResponse (Execute r) -> Just r.depth
            _other -> Nothing
