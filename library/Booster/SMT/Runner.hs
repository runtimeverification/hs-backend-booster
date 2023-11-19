{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}

module Booster.SMT.Runner (
    SMTContext (..),
    SMT (..),
    mkContext,
    closeContext,
    runSMT,
    declare,
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.Proxy
import SMTLIB.Backends qualified as Backend
import SMTLIB.Backends.Process qualified as Backend
import System.IO (Handle, IOMode (..), hClose, hSetBuffering, BufferMode (..), hSetBinaryMode, openFile)

import Booster.SMT.Base

data SMTContext = SMTContext
    { solver :: Backend.Solver
    , solverClose :: IO ()
    , mbTranscript :: Maybe Handle
    }

mkContext ::
    -- KoreDefinition ->
    Maybe FilePath ->
    IO SMTContext
mkContext transcriptPath = do
    mbTranscript <-
        forM transcriptPath $ \path -> do
            h <- openFile path WriteMode
            hSetBuffering h (BlockBuffering Nothing)
            hSetBinaryMode h True
            BS.hPutStrLn h "; starting solver process"
            pure h

    let config =
            Backend.defaultConfig
                { Backend.std_err = maybe Backend.Inherit Backend.UseHandle mbTranscript
                }
    handle <- Backend.new config
    let solverClose = Backend.close handle
    solver <- Backend.initSolver Backend.Queuing $ Backend.toBackend handle
    pure SMTContext
        { solver
        , solverClose
        , mbTranscript
        }

closeContext :: SMTContext -> IO ()
closeContext ctxt = do
    whenJust ctxt.mbTranscript $ hClose
    ctxt.solverClose

newtype SMT m a = SMT (ReaderT SMTContext m a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

runSMT :: SMTContext -> SMT io a -> io a
runSMT ctxt (SMT action) =
    runReaderT action ctxt

declare :: MonadLoggerIO io => [DeclareCommand] -> SMT io ()
declare cs = mapM_ runCmd cs

-- TODO move this to its own file!
class SMTEncode cmd where
    encode :: cmd -> BS.Builder

    -- selecting the actual runner (command_ for Declare and Control, command for query)
    run_ :: MonadLoggerIO io =>
            Proxy cmd -> Backend.Solver -> BS.Builder -> SMT io BS.Builder

runCmd :: forall cmd io . (SMTEncode cmd, MonadLoggerIO io) => cmd -> SMT io Response
runCmd cmd = do
        let cmdBS = encode cmd
        ctxt <- SMT ask
        whenJust ctxt.mbTranscript $ \h ->
            liftIO (BS.hPutBuilder h $ cmdBS)
        result <- decode <$> run_ @cmd Proxy ctxt.solver cmdBS
        whenJust ctxt.mbTranscript $
            liftIO . flip BS.hPutStrLn (BS.pack $ show result)
        pure result

instance SMTEncode DeclareCommand where

    encode = const $ BS.shortByteString ""

    run_ _ s = fmap (const "(Success)") . liftIO . Backend.command_ s

decode :: BS.Builder -> Response
decode = const $ Error "Decoder not implemented"
