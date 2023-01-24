{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Kore.LLVM.Internal (API (..), KorePatternAPI (..), runLLVM, withDLib, mkAPI, ask, marshallTerm, marshallSort) where

import Control.Monad (foldM, forM_, void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (packCStringLen)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign qualified
import Foreign.C qualified as C
import Foreign.C.Types (CSize (..))
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Kore.LLVM.TH (dynamicBindings)
import Kore.Pattern.Base
import Kore.Pattern.Util (sortOfTerm)
import System.Posix.DynamicLinker qualified as Linker

data KorePattern
data KoreSort
data KoreSymbol
data Block
type SizeT = CSize

type KorePatternPtr = ForeignPtr KorePattern
type KoreSymbolPtr = ForeignPtr KoreSymbol
type KoreSortPtr = ForeignPtr KoreSort

$(dynamicBindings "./cbits/kllvm-c.h")

newtype KoreStringPatternAPI = KoreStringPatternAPI
    { new :: Text -> IO KorePatternPtr
    }

newtype KoreTokenPatternAPI = KoreTokenPatternAPI
    { new :: Text -> KoreSortPtr -> IO KorePatternPtr
    }

data KoreSymbolAPI = KoreSymbolAPI
    { new :: Text -> IO KoreSymbolPtr
    , addArgument :: KoreSymbolPtr -> KoreSortPtr -> IO KoreSymbolPtr
    }

data KoreSortAPI = KoreSortAPI
    { new :: Text -> IO KoreSortPtr
    , addArgument :: KoreSortPtr -> KoreSortPtr -> IO KoreSortPtr
    , dump :: KoreSortPtr -> IO String
    }

data KorePatternAPI = KorePatternAPI
    { new :: Text -> IO KorePatternPtr
    , addArgument :: KorePatternPtr -> KorePatternPtr -> IO KorePatternPtr
    , fromSymbol :: KoreSymbolPtr -> IO KorePatternPtr
    , string :: KoreStringPatternAPI
    , token :: KoreTokenPatternAPI
    , dump :: KorePatternPtr -> IO String
    }

data API = API
    { patt :: KorePatternAPI
    , symbol :: KoreSymbolAPI
    , sort :: KoreSortAPI
    , simplifyBool :: KorePatternPtr -> IO Bool
    , simplify :: KorePatternPtr -> KoreSortPtr -> IO ByteString
    }

newtype LLVM a = LLVM (ReaderT API IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

{- | Uses dlopen to load a .so/.dylib C library at runtime. For doucmentation of flags such as `RTL_LAZY`, consult e.g.
     https://man7.org/linux/man-pages/man3/dlopen.3.html
-}
withDLib :: FilePath -> (Linker.DL -> IO a) -> IO a
withDLib dlib = Linker.withDL dlib [Linker.RTLD_LAZY]

runLLVM :: API -> LLVM a -> IO a
runLLVM api (LLVM m) = runReaderT m api

mkAPI :: Linker.DL -> IO API
mkAPI dlib = flip runReaderT dlib $ do
    freePattern <- {-# SCC "LLVM.pattern.free" #-} korePatternFreeFunPtr

    newCompositePattern <- koreCompositePatternNew
    let new name =
            {-# SCC "LLVM.pattern.new" #-}
            liftIO $
                C.withCString (Text.unpack name) $
                    newCompositePattern >=> newForeignPtr freePattern

    addArgumentCompositePattern <- koreCompositePatternAddArgument
    let addArgument parent child =
            liftIO $
                {-# SCC "LLVM.pattern.addArgument" #-}
                do
                    withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentCompositePattern rawParent
                    finalizeForeignPtr child
                    pure parent

    newString <- koreStringPatternNewWithLen
    let string = KoreStringPatternAPI $ \name ->
            {-# SCC "LLVM.pattern.string" #-}
            liftIO $ C.withCStringLen (Text.unpack name) $ \(rawStr, len) ->
                newString rawStr (fromIntegral len) >>= newForeignPtr freePattern

    newToken <- korePatternNewTokenWithLen
    let token = KoreTokenPatternAPI $ \name sort ->
            {-# SCC "LLVM.pattern.token" #-}
            liftIO $ C.withCStringLen (Text.unpack name) $ \(rawName, len) ->
                withForeignPtr sort $
                    newToken rawName (fromIntegral len) >=> newForeignPtr freePattern

    compositePatternFromSymbol <- koreCompositePatternFromSymbol
    let fromSymbol sym = {-# SCC "LLVM.pattern.fromSymbol" #-} liftIO $ withForeignPtr sym $ compositePatternFromSymbol >=> newForeignPtr freePattern

    dump' <- korePatternDump
    let dump ptr =
            {-# SCC "LLVM.pattern.dump" #-}
            liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dump' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str
    let patt = KorePatternAPI{new, addArgument, string, token, fromSymbol, dump}

    freeSymbol <- {-# SCC "LLVM.symbol.free" #-} koreSymbolFreeFunPtr

    newSymbol <- koreSymbolNew
    let new name =
            {-# SCC "LLVM.symbol.new" #-}
            liftIO $
                C.withCString (Text.unpack name) $
                    newSymbol >=> newForeignPtr freeSymbol

    addArgumentSymbol <- koreSymbolAddFormalArgument
    let addArgument sym sort =
            liftIO $
                {-# SCC "LLVM.symbol.addArgument" #-}
                do
                    withForeignPtr sym $ \rawSym -> withForeignPtr sort $ addArgumentSymbol rawSym
                    pure sym

    let symbol = KoreSymbolAPI{new, addArgument}

    freeSort <- {-# SCC "LLVM.sort.free" #-} koreSortFreeFunPtr

    newSort <- koreCompositeSortNew
    let new name =
            {-# SCC "LLVM.sort.new" #-}
            liftIO $
                C.withCString (Text.unpack name) $
                    newSort >=> newForeignPtr freeSort

    addArgumentSort <- koreCompositeSortAddArgument
    let addArgument parent child =
            liftIO $
                {-# SCC "LLVM.sort.addArgument" #-}
                do
                    withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentSort rawParent
                    pure parent

    dump' <- koreSortDump
    let dump ptr =
            {-# SCC "LLVM.sort.dump" #-}
            liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dump' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

    let sort = KoreSortAPI{new, addArgument, dump}

    simplifyBool' <- koreSimplifyBool
    let simplifyBool p = {-# SCC "LLVM.simplifyBool" #-} liftIO $ withForeignPtr p $ fmap (== 1) <$> simplifyBool'

    simplify' <- koreSimplify
    let simplify pat srt =
            {-# SCC "LLVM.simplify" #-}
            liftIO $
                withForeignPtr pat $ \patPtr ->
                    withForeignPtr srt $ \sortPtr ->
                        alloca $ \lenPtr ->
                            alloca $ \strPtr -> do
                                simplify' patPtr sortPtr strPtr lenPtr
                                len <- fromIntegral <$> peek lenPtr
                                cstr <- peek strPtr
                                packCStringLen (cstr, len)

    pure API{patt, symbol, sort, simplifyBool, simplify}

ask :: LLVM API
ask = LLVM Reader.ask

marshallSymbol :: Symbol -> [Sort] -> LLVM KoreSymbolPtr
marshallSymbol sym sorts = do
    kore <- ask
    sym' <- liftIO $ kore.symbol.new sym.name
    foldM (\symbol sort -> marshallSort sort >>= liftIO . kore.symbol.addArgument symbol) sym' sorts

marshallSort :: Sort -> LLVM KoreSortPtr
marshallSort = \case
    SortApp name args -> do
        kore <- ask
        sort <- liftIO $ kore.sort.new name
        forM_ args $ marshallSort >=> liftIO . kore.sort.addArgument sort
        pure sort
    SortVar varName -> error $ "marshalling SortVar " <> show varName <> " unsupported"

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm t = do
    kore <- ask
    case t of
        SymbolApplication symbol sorts trms -> do
            trm <- liftIO . kore.patt.fromSymbol =<< marshallSymbol symbol sorts
            forM_ trms $ marshallTerm >=> liftIO . kore.patt.addArgument trm
            pure trm
        AndTerm l r -> do
            andSym <- liftIO $ kore.symbol.new "\\and"
            void $ liftIO . kore.symbol.addArgument andSym =<< marshallSort (sortOfTerm l)
            trm <- liftIO $ kore.patt.fromSymbol andSym
            void $ liftIO . kore.patt.addArgument trm =<< marshallTerm l
            liftIO . kore.patt.addArgument trm =<< marshallTerm r
        DomainValue sort val ->
            marshallSort sort >>= liftIO . kore.patt.token.new val
        Var varName -> error $ "marshalling Var " <> show varName <> " unsupported"
