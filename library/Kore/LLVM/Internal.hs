module Kore.LLVM.Internal where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Foreign (ForeignPtr, FunPtr, Ptr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C qualified as C
import Kore.Pattern.Base
import System.Posix qualified as Linker

data KORECompositePattern
type KORECompositePatternRawPtr = Ptr KORECompositePattern

type KoreCompositePatternNew = C.CString -> IO KORECompositePatternRawPtr
type KoreCompositePatternAddArgument = KORECompositePatternRawPtr -> KORECompositePatternRawPtr -> IO ()
type KoreCompositePatternDump = KORECompositePatternRawPtr -> IO ()

type KORECompositePatternPtr = ForeignPtr KORECompositePattern

foreign import ccall "dynamic" mkKoreCompositePatternNew :: FunPtr KoreCompositePatternNew -> KoreCompositePatternNew
foreign import ccall "dynamic" mkKoreCompositePatternAddArgument :: FunPtr KoreCompositePatternAddArgument -> KoreCompositePatternAddArgument
foreign import ccall "dynamic" mkKoreCompositePatternDump :: FunPtr KoreCompositePatternDump -> KoreCompositePatternDump

data KoreCompositePattern = KoreCompositePattern
    { new :: Text -> LLVM KORECompositePatternPtr
    , addArgument :: KORECompositePatternPtr -> KORECompositePatternPtr -> LLVM KORECompositePatternPtr
    , dump :: KORECompositePatternPtr -> LLVM ()
    }

data API = API
    { koreCompositePattern :: KoreCompositePattern
    }

newtype LLVM a = LLVM {unLLVM :: ReaderT API IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

runLLVM :: FilePath -> LLVM a -> IO a
runLLVM dlib (LLVM m) = do
    Linker.withDL dlib [Linker.RTLD_LAZY] $ \libHandle -> do
        new' <- mkKoreCompositePatternNew <$> Linker.dlsym libHandle "kore_composite_pattern_new"
        free <- Linker.dlsym libHandle "kore_composite_pattern_free"
        let new name = liftIO $ Text.withCString name $ \cname -> newForeignPtr free =<< new' cname

        dump' <- mkKoreCompositePatternDump <$> Linker.dlsym libHandle "kore_composite_pattern_dump"
        let dump ptr = liftIO $ withForeignPtr ptr $ dump'

        addArgument' <- mkKoreCompositePatternAddArgument <$> Linker.dlsym libHandle "kore_composite_pattern_add_argument"

        let addArgument parent child = liftIO $ do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgument' rawParent
                finalizeForeignPtr child
                pure parent

        liftIO $ runReaderT m $ API KoreCompositePattern{new, dump, addArgument}

ask :: LLVM API
ask = LLVM Reader.ask

marshallTerm :: Term -> LLVM KORECompositePatternPtr
marshallTerm = \case
    SymbolApplication symbol trms -> do
        api <- ask
        app <- api.koreCompositePattern.new symbol.name
        foldM (\app' t -> api.koreCompositePattern.addArgument app' =<< marshallTerm t) app trms
    AndTerm _ _ -> error "marshalling And undefined"
    DomainValue _ _ -> error "marshalling DomainValue undefined"
    Var _ -> error "marshalling Var undefined"
