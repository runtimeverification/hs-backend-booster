{-# LANGUAGE DeriveFunctor #-}

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
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BS
import SMTLIB.Backends qualified as Backend
import SMTLIB.Backends.Process qualified as Backend
import System.IO (Handle, IOMode (..), hClose, openFile)

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

runSMT :: MonadLoggerIO io => SMTContext -> SMT io a -> io a
runSMT ctxt (SMT action) = runReaderT action ctxt
