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

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.BZip qualified as BZ2
import Codec.Compression.GZip qualified as GZip
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json
import Data.Aeson.Key qualified as JsonKey
import Data.Aeson.KeyMap qualified as JsonKeyMap
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (isDigit, toLower)
import Data.Int (Int64)
import Data.List.Extra
import Data.Maybe (isNothing, mapMaybe)
import Data.Text qualified as Text
import Data.Vector as Array (fromList)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Options.Applicative
import System.Clock
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Extra
import System.Process

import Booster.JsonRpc (rpcJsonConfig)
import Booster.JsonRpc.Utils (diffJson, isIdentical, renderResult)
import Booster.Syntax.Json qualified as Syntax

main :: IO ()
main = do
    Options{common, runOptions} <- execParser parseOptions
    case runOptions of
        RunTarball tarFile keepGoing ->
            runTarball common tarFile keepGoing
        RunSingle mode optionFile options processingOptions -> do
            let ProcessingOptions{postProcessing, prettify, time, dryRun} = processingOptions
            request <- withLogLevel common.logLevel $ do
                logInfo_ "Preparing request data"
                request <- prepareRequestData mode optionFile options
                when dryRun $ do
                    logInfo_ "Dry-run mode, just showing request instead of sending"
                    let write :: BS.ByteString -> LoggingT IO ()
                        write
                            | Just (Expect True file) <- postProcessing = \contents -> do
                                logInfo_ ("Writing request to file " <> file)
                                liftIO $ BS.writeFile file contents
                            | otherwise = liftIO . BS.putStrLn
                        reformat = Json.encodePretty' rpcJsonConfig . Json.decode @Json.Value
                    write $ if not prettify then request else reformat request
                    liftIO exitSuccess
                pure request
            -- runTCPClient operates on IO directly, therefore repeating runStderrLogging
            runTCPClient common.host (show common.port) $ \s ->
                cancelIfInterrupted s $ do
                    withLogLevel common.logLevel $
                        makeRequest
                            time
                            (getModeFile mode)
                            s
                            8192
                            request
                            (postProcess prettify postProcessing)
                    shutdown s ShutdownReceive

makeRequest ::
    MonadLoggerIO m =>
    Bool ->
    String ->
    Socket ->
    Int64 ->
    BS.ByteString ->
    (BS.ByteString -> m a) ->
    m a
makeRequest time name s bufSize request handleResponse = do
    start <- liftIO $ getTime Monotonic
    logInfo_ $ "Sending request " <> name <> "..."
    logDebug_ $ "Request JSON: " <> BS.unpack request
    liftIO $ sendAll s request
    response <- liftIO readResponse
    end <- liftIO $ getTime Monotonic
    logInfo_ "Response received."
    let timeStr = timeSpecs start end
    logInfo_ $ "Round trip time for request '" <> name <> "' was " <> timeStr
    when time $ do
        logInfo_ $ "Saving timing for " <> name
        liftIO $ writeFile (name <> ".time") timeStr
    handleResponse response
  where
    readResponse :: IO BS.ByteString
    readResponse = do
        part <- recv s bufSize
        if BS.length part < bufSize
            then pure part
            else do
                more <- readResponse
                pure $ part <> more

cancelIfInterrupted :: Socket -> IO () -> IO ()
cancelIfInterrupted skt operation =
    catchJust isInterrupt operation $ const sendCancel
  where
    isInterrupt :: AsyncException -> Maybe ()
    isInterrupt UserInterrupt = Just ()
    isInterrupt _other = Nothing

    sendCancel = do
        sendAll skt (Json.encode cancel)
        shutdown skt ShutdownReceive
        hPutStrLn stderr "Interrupted"
        exitWith (ExitFailure 130)

    cancel = object ["jsonrpc" ~> "2.0", "id" ~> "1", "method" ~> "cancel"]

----------------------------------------
-- Logging

-- Logging functions without location
logError_, logWarn_, logInfo_, logDebug_ :: MonadLoggerIO m => String -> m ()
logError_ = logWithoutLoc "" LevelError
logWarn_ = logWithoutLoc "" LevelWarn
logInfo_ = logWithoutLoc "" LevelInfo
logDebug_ = logWithoutLoc "" LevelDebug

withLogLevel :: LogLevel -> LoggingT IO a -> IO a
withLogLevel lvl = runStderrLoggingT . filterLogger (const (>= lvl))

----------------------------------------
-- program options

data Options = Options
    { common :: CommonOptions
    , runOptions :: RunOptions
    }
    deriving stock (Show)

data CommonOptions = CommonOptions
    { host :: String
    , port :: Int
    , logLevel :: LogLevel
    }
    deriving stock (Show)

data RunOptions
    = -- | run a single request
      RunSingle
        Mode -- what kind of request
        (Maybe FilePath) -- json file with options
        [(String, String)] -- verbatim options (name, value) to add to json
        ProcessingOptions
    | -- | run all requests contained in a tarball (using some conventions)
      RunTarball
        FilePath -- tar file
        Bool -- do not stop on first diff if set to true
    deriving stock (Show)

data ProcessingOptions = ProcessingOptions
    { postProcessing :: Maybe PostProcessing
    , prettify :: Bool
    , time :: Bool
    , dryRun :: Bool
    }
    deriving stock (Show)

{- | Defines details for a single requests. Either assemble a request
 from the state in a file and given options in a file or on the
 command line, or raw data (entire input in a file) without additional
 options.
-}
data Mode
    = Exec FilePath
    | Simpl FilePath
    | AddModule FilePath
    | GetModel FilePath
    | Check FilePath FilePath
    | SendRaw FilePath
    deriving stock (Show)

getModeFile :: Mode -> FilePath
getModeFile = \case
    Exec f -> f
    Simpl f -> f
    AddModule f -> f
    GetModel f -> f
    Check f1 _ -> f1
    SendRaw f -> f

{- | Optional output post-processing:
  * 'Expect' checks formatted output against a given golden file.
  * If `regenerate` is set to true, will create/overrie the expected file with received output
-}
data PostProcessing = Expect
    { regenerate :: Bool
    , expectFile :: FilePath
    }
    deriving stock (Show)

parseCommonOptions :: Parser CommonOptions
parseCommonOptions =
    CommonOptions
        <$> strOption
            ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value "localhost"
                <> help "server host to connect to"
                <> showDefault
            )
        <*> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 31337
                <> help "server port to connect to"
                <> showDefault
            )
        <*> option
            readLogLevel
            ( long "log-level"
                <> short 'l'
                <> metavar "LOG_LEVEL"
                <> value LevelInfo
                <> help "Log level, one of [Error, Warn, Info, Debug]"
                <> showDefault
            )
  where
    readLogLevel :: ReadM LogLevel
    readLogLevel =
        eitherReader $ \s -> case map toLower s of
            "debug" -> Right LevelDebug
            "info" -> Right LevelInfo
            "warn" -> Right LevelWarn
            "error" -> Right LevelError
            _other -> Left $ s <> ": Unsupported log level"

parseOptions :: ParserInfo Options
parseOptions =
    info
        (parseOptions' <**> helper)
        ( fullDesc
            <> progDesc "Simple RPC test client"
        )
  where
    parseOptions' =
        Options
            <$> parseCommonOptions
            <*> parseMode

parseProcessingOptions :: Parser ProcessingOptions
parseProcessingOptions =
    ProcessingOptions
        <$> optional parsePostProcessing
        <*> prettifyOpt
        <*> timeOpt
        <*> dryRunOpt
  where
    flagOpt name desc = switch $ long name <> help desc
    prettifyOpt = flagOpt "prettify" "format JSON before printing"
    timeOpt = flagOpt "time" "record the timing information between sending a request and receiving a response"
    dryRunOpt = flagOpt "dry-run" "Do not send anything, just output the request"

parsePostProcessing :: Parser PostProcessing
parsePostProcessing =
    ( Expect
        <$> ( flag False True $
                long "regenerate"
                    <> help "regenerate the expected file"
            )
        <*> strOption
            ( long "expect"
                <> metavar "EXPECTATIONFILE"
                <> help "compare JSON output against file contents"
            )
    )
        <|> ( Expect True
                <$> ( strOption $
                        long "output"
                            <> short 'o'
                            <> metavar "OUTPUTFILE"
                            <> help "write JSON output to a file"
                    )
            )

parseMode :: Parser RunOptions
parseMode =
    subparser
        ( command
            "send"
            ( info
                ( RunSingle
                    <$> (SendRaw <$> strArgument (metavar "FILENAME"))
                    <*> pure Nothing -- no param file
                    <*> pure [] -- no params
                    <*> parseProcessingOptions
                    <**> helper
                )
                (progDesc "send the raw file contents directly")
            )
            <> command
                "execute"
                ( info
                    ( RunSingle
                        <$> (Exec <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "execute (rewrite) the state in the file")
                )
            <> command
                "simplify"
                ( info
                    ( RunSingle
                        <$> (Simpl <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "simplify the state or condition in the file")
                )
            <> command
                "add-module"
                ( info
                    ( RunSingle
                        <$> (AddModule <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "add the module in the given kore file")
                )
            <> command
                "get-model"
                ( info
                    ( RunSingle
                        <$> (GetModel <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "check satisfiability/provide model for the state in the file")
                )
            <> command
                "run-tarball"
                ( info
                    ( RunTarball
                        <$> strArgument (metavar "FILENAME")
                        <*> switch (long "keep-going" <> help "do not stop on unexpected output")
                        <**> helper
                    )
                    (progDesc "Run all requests and compare responses from a bug report tarball")
                )
        )
  where
    paramFileOpt =
        optional $
            strOption $
                long "param-file"
                    <> metavar "PARAMFILE"
                    <> help "file with parameters (json object), optional"
    paramOpt =
        option readPair $
            short 'O'
                <> metavar "NAME=VALUE"
                <> help "parameters to use (name=value)"
    readPair =
        maybeReader $ \s -> case split (== '=') s of [k, v] -> Just (k, v); _ -> Nothing

----------------------------------------
-- Running all requests contained in the `rpc_*` directory of a tarball

runTarball :: CommonOptions -> FilePath -> Bool -> IO ()
runTarball common tarFile keepGoing = do
    -- unpack tar files, determining type from extension(s)
    let unpackTar
            | ".tar" == takeExtension tarFile = Tar.read
            | ".tgz" == takeExtension tarFile = Tar.read . GZip.decompress
            | ".tar.gz" `isSuffixOf` takeExtensions tarFile = Tar.read . GZip.decompress
            | ".tar.bz2" `isSuffixOf` takeExtensions tarFile = Tar.read . BZ2.decompress
            | otherwise = Tar.read

    containedFiles <- unpackTar <$> BS.readFile tarFile
    let checked = Tar.checkSecurity containedFiles
    -- probe server connection before doing anything, display
    -- instructions unless server was found.
    runTCPClient common.host (show common.port) (runAllRequests checked)
        `catch` (withLogLevel common.logLevel . noServerError)
  where
    runAllRequests ::
        Tar.Entries (Either Tar.FormatError Tar.FileNameError) -> Socket -> IO ()
    runAllRequests checked skt = cancelIfInterrupted skt $ do
        withTempDir $ \tmp -> withLogLevel common.logLevel $ do
            -- unpack relevant tar files (rpc_* directories only)
            logInfo_ $ unwords ["unpacking json files from tarball", tarFile, "into", tmp]
            jsonFiles <-
                liftIO $ Tar.foldEntries (unpackIfRpc tmp) (pure []) throwAnyError checked
            logInfo_ $ "RPC data:" <> show jsonFiles

            let requests = mapMaybe (stripSuffix "_request.json") jsonFiles
            results <-
                forM requests $ \r -> do
                    mbError <- runRequest skt tmp jsonFiles r
                    case mbError of
                        Just err -> do
                            logError_ $ "Request " <> r <> " failed: " <> BS.unpack err
                            unless keepGoing $
                                liftIO $
                                    shutdown skt ShutdownReceive >> exitWith (ExitFailure 2)
                        Nothing ->
                            logInfo_ $ "Response to " <> r <> " matched with expected"
                    pure mbError
            liftIO $ shutdown skt ShutdownReceive
            liftIO $ exitWith (if all isNothing results then ExitSuccess else ExitFailure 2)

    -- complain on any errors in the tarball
    throwAnyError :: Either Tar.FormatError Tar.FileNameError -> IO a
    throwAnyError = either throwIO throwIO

    -- unpack all rpc_*/*.json files into dir and return their names
    unpackIfRpc :: FilePath -> Tar.Entry -> IO [FilePath] -> IO [FilePath]
    unpackIfRpc tmpDir entry acc = do
        case splitFileName (Tar.entryPath entry) of
            -- assume single directory "rpc_<something>" containing "*.json" files
            (_, "") -- skip all directories
                | Tar.Directory <- Tar.entryContent entry ->
                    acc
            (dir, file) -- unpack json files into tmp directory
                | "rpc_" `isPrefixOf` dir
                , ".json" `isSuffixOf` file
                , Tar.NormalFile bs _size <- Tar.entryContent entry -> do
                    BS.writeFile (tmpDir </> file) bs
                    (file :) <$> acc
            _other ->
                -- skip anything else
                acc

    noServerError :: MonadLoggerIO m => IOException -> m ()
    noServerError e@IOError{ioe_type = NoSuchThing} = do
        -- show instructions how to run the server
        logError_ $ "Could not connect to RPC server on port " <> show common.port
        logError_ $ show e
        logError_ $
            unlines
                [ ""
                , "To run the required RPC server, you need to"
                , "1) extract `definition.kore` and `server_instance.json` from the tarball;"
                , "2) look up the module name `<MODULE>` in `server_instance.json`;"
                , "3) and then run the server using"
                , "   $ kore-rpc definition.kore --module <MODULE> --server-port " <> show common.port
                , ""
                , "If you want to use `kore-rpc-booster, you should also compile an LLVM backend library"
                , "by 1) extracting the `llvm_definition/` directory from the tarball;"
                , "   2) running the llvm backend compiler on the unpacked files:"
                , "      $ llvm-kompile llvm_definition/definition.kore llvm_definition/dt c -- -o interpreter`"
                , "This will generate `interpreter.[so|dylib]` and you can run"
                , "  `kore-rpc-booster definition.kore --main-module <MODULE> --llvm-backend-library interpreter.so`"
                ]
        liftIO $ exitWith (ExitFailure 1)
    noServerError otherError = do
        logError_ $ show otherError
        liftIO $ exitWith (ExitFailure 1)

    -- Runs one request, checking that a response is available for
    -- comparison. Returns Nothing if successful (identical
    -- response), or rendered diff or error message if failing
    runRequest ::
        MonadLoggerIO m => Socket -> FilePath -> [FilePath] -> String -> m (Maybe BS.ByteString)
    runRequest skt tmpDir jsonFiles basename
        | not . (`elem` jsonFiles) $ basename <> "_response.json" =
            pure . Just . BS.pack $ "Response file " <> basename <> "_response.json is missing."
        | not . (`elem` jsonFiles) $ basename <> "_request.json" =
            pure . Just . BS.pack $ "Request file " <> basename <> "_request.json is missing."
        | otherwise = do
            request <- liftIO . BS.readFile $ tmpDir </> basename <> "_request.json"
            expected <- liftIO . BS.readFile $ tmpDir </> basename <> "_response.json"

            actual <- makeRequest False basename skt 8192 request pure

            let diff = diffJson expected actual
            if isIdentical diff
                then pure Nothing
                else pure . Just $ renderResult "expected response" "actual response" diff

----------------------------------------
prepareRequestData ::
    MonadLoggerIO m => Mode -> Maybe FilePath -> [(String, String)] -> m BS.ByteString
prepareRequestData (SendRaw file) mbFile opts = do
    unless (isNothing mbFile) $
        logWarn_ "Raw mode, ignoring given option file"
    unless (null opts) $
        logWarn_ "Raw mode, ignoring given request options"
    liftIO $ BS.readFile file
prepareRequestData (Exec file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "execute" file mbOptFile opts
prepareRequestData (Simpl file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "simplify" file mbOptFile opts
prepareRequestData (AddModule file) mbOptFile opts = do
    unless (isNothing mbOptFile) $
        logWarn_ "Add-module mode, ignoring given option file"
    unless (null opts) $
        logWarn_ "Raw mode, ignoring given request options"
    moduleText <- liftIO $ readFile file
    pure . Json.encode $
        object
            [ "jsonrpc" ~> "2.0"
            , "id" ~> "1"
            , "method" ~> "add-module"
            ]
            +: "params"
            ~> Json.Object (object ["module" ~> moduleText])
prepareRequestData (GetModel file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "get-model" file mbOptFile opts
prepareRequestData (Check _file1 _file2) _mbOptFile _opts = do
    error "not implemented yet"

prepareOneTermRequest ::
    String -> FilePath -> Maybe FilePath -> [(String, String)] -> IO BS.ByteString
prepareOneTermRequest method file mbOptFile opts = do
    term :: Json.Value <-
        Json.toJSON
            <$> ( BS.readFile file -- decode given term to test whether it is valid
                    >>= either error pure . Json.eitherDecode @Syntax.KoreJson
                )
    paramsFromFile <-
        maybe
            (pure JsonKeyMap.empty)
            ( BS.readFile
                >=> either error (pure . getObject) . Json.eitherDecode @Json.Value
            )
            mbOptFile
    let params = paramsFromFile <> object opts
    let requestData =
            object
                [ "jsonrpc" ~> "2.0"
                , "id" ~> "1"
                , "method" ~> method
                ]
                +: "params"
                ~> Json.Object (params +: "state" ~> term)
    pure $ Json.encode requestData

getObject :: Json.Value -> Json.Object
getObject (Json.Object o) = o
getObject other = error $ "Expected object, found " <> show other

object :: [(String, String)] -> Json.Object
object = JsonKeyMap.fromList . map mkKeyPair
  where
    mkKeyPair = bimap JsonKey.fromString valueFrom

    -- second-guessing the value type from the contents
    -- we need single-word strings, lists of strings, and numbers
    valueFrom :: String -> Json.Value
    valueFrom [] = Json.Null
    valueFrom s@('[' : rest)
        | last rest == ']' =
            Json.Array $ valuesFrom (init rest)
        | otherwise =
            error $ "garbled list " <> s
    valueFrom s
        | all isDigit s =
            Json.Number (fromInteger $ read s)
    valueFrom s =
        Json.String $ Text.pack s

    -- comma-separated list of values
    valuesFrom :: String -> Json.Array
    valuesFrom = Array.fromList . map (valueFrom . trim) . split (== ',')

infixl 5 ~>
(~>) :: k -> v -> (k, v)
(~>) = (,)

infixl 4 +:
(+:) :: Json.Object -> (String, Json.Value) -> Json.Object
o +: (k, v) = JsonKeyMap.insert (JsonKey.fromString k) v o

postProcess ::
    MonadLoggerIO m => Bool -> Maybe PostProcessing -> BS.ByteString -> m ()
postProcess prettify postProcessing output =
    case postProcessing of
        Nothing ->
            liftIO $ BS.putStrLn $ if prettify then prettyOutput else output
        Just Expect{expectFile, regenerate} -> do
            liftIO (doesFileExist expectFile) >>= \case
                False ->
                    if regenerate
                        then do
                            logInfo_ $ "Writing file " <> expectFile <> " for the first time."
                            liftIO $ BS.writeFile expectFile prettyOutput
                        else do
                            logError_ $
                                "The expected file "
                                    <> expectFile
                                    <> " does not exist. Use `--regenerate` if you wish to create it."
                            liftIO . exitWith $ ExitFailure 1
                True -> do
                    expected <- liftIO $ BS.readFile expectFile
                    when (prettyOutput /= expected) $ do
                        liftIO $ BS.writeFile "response" prettyOutput
                        (_, result, _) <-
                            liftIO $
                                readProcessWithExitCode "git" ["diff", "--no-index", "--color-words=.", expectFile, "response"] ""
                        liftIO $ putStrLn result

                        if regenerate
                            then do
                                logInfo_ $ "Re-generating expected file " <> expectFile
                                liftIO $ renameFile "response" expectFile
                            else do
                                liftIO $ removeFile "response"
                                logError_ $ "Response differs from expected " <> expectFile
                                liftIO . exitWith $ ExitFailure 1
                    logInfo_ $ "Output matches " <> expectFile
  where
    prettyOutput =
        Json.encodePretty' rpcJsonConfig $
            either error (id @Json.Value) $
                Json.eitherDecode output

timeSpecs :: TimeSpec -> TimeSpec -> String
timeSpecs = fmt0
  where
    fmt diff
        | Just i <- scale (10 ^ (9 :: Int)) = show i <> " s"
        | Just i <- scale (10 ^ (6 :: Int)) = show i <> " ms"
        | Just i <- scale (10 ^ (3 :: Int)) = show i <> " us"
        | otherwise = show diff <> " ns"
      where
        scale :: Integer -> Maybe Double
        scale i =
            if diff >= i
                then Just (fromIntegral diff / fromIntegral i)
                else Nothing

    fmt0 (TimeSpec s1 n1) (TimeSpec s2 n2) = fmt diff
      where
        diff :: Integer
        diff = a2 - a1
        a1 = (fromIntegral s1 * 10 ^ (9 :: Int)) + fromIntegral n1
        a2 = (fromIntegral s2 * 10 ^ (9 :: Int)) + fromIntegral n2
