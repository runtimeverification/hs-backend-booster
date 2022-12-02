{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Kore.LLVM.Internal (API (..), KorePatternAPI (..), runLLVM, ask, marshallTerm) where

import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign qualified
import Foreign.C qualified as C
import Kore.LLVM.TH (dynamicBindings)
import Kore.Pattern.Base
import System.Posix.DynamicLinker qualified as Linker

data KorePattern
type KorePatternPtr = ForeignPtr KorePattern

$(dynamicBindings "./cbits/kllvm-c.h")

newtype KoreCompositePatternAPI = KoreCompositePatternAPI
    { new :: Text -> LLVM KorePatternPtr
    }

newtype KoreStringPatternAPI = KoreStringPatternAPI
    { new :: Text -> LLVM KorePatternPtr
    }

data KorePatternAPI = KorePatternAPI
    { composite :: KoreCompositePatternAPI
    , string :: KoreStringPatternAPI
    , addArgument :: KorePatternPtr -> KorePatternPtr -> LLVM KorePatternPtr
    , dump :: KorePatternPtr -> LLVM String
    }

newtype API = API
    { korePattern :: KorePatternAPI
    }

newtype LLVM a = LLVM (ReaderT API IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

runLLVM :: FilePath -> LLVM a -> IO a
runLLVM dlib (LLVM m) = do
    Linker.withDL dlib [Linker.RTLD_LAZY] $ \libHandle -> flip runReaderT libHandle $ do
        free <- korePatternFreeFunPtr
        composite <- do
            new <- koreCompositePatternNew
            pure $ KoreCompositePatternAPI $ \name -> liftIO $ C.withCString (Text.unpack name) $ new >=> newForeignPtr free

        string <- do
            new <- koreStringPatternNew
            pure $ KoreStringPatternAPI $ \name -> liftIO $ C.withCString (Text.unpack name) $ new >=> newForeignPtr free

        dump <- do
            dump' <- korePatternDump
            pure $ \ptr -> liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dump' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

        addArgument <- do
            addArgument' <- koreCompositePatternAddArgument
            pure $ \parent child -> liftIO $ do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgument' rawParent
                finalizeForeignPtr child
                pure parent

        liftIO $ runReaderT m $ API KorePatternAPI{composite, string, addArgument, dump}

ask :: LLVM API
ask = LLVM Reader.ask

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm = \case
    SymbolApplication symbol trms -> do
        api <- ask
        app <- api.korePattern.composite.new symbol.name
        foldM (\app' t -> api.korePattern.addArgument app' =<< marshallTerm t) app trms
    AndTerm _ _ -> error "marshalling And undefined"
    DomainValue _sort val -> do
        api <- ask
        api.korePattern.string.new val
    Var _ -> error "marshalling Var undefined"
