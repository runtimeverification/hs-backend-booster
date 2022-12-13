{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Kore.LLVM.Internal (API (..), KorePatternAPI (..), runLLVM, runLLVMwithDL, withDLib, ask, marshallTerm) where

import Control.Monad (forM_, (>=>), join, void)
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
import Kore.Pattern.Util(sortOfTerm)
import System.Posix.DynamicLinker qualified as Linker

data KorePattern
data KoreSort
data KoreSymbol
data Block

type KorePatternPtr = ForeignPtr KorePattern
type KoreSymbolPtr = ForeignPtr KoreSymbol
type KoreSortPtr = ForeignPtr KoreSort

$(dynamicBindings "./cbits/kllvm-c.h")


newtype KoreStringPatternAPI = KoreStringPatternAPI
    { new :: Text -> LLVM KorePatternPtr
    }

newtype KoreTokenPatternAPI = KoreTokenPatternAPI
    { new :: Text -> KoreSortPtr -> LLVM KorePatternPtr
    }

data KoreSymbolAPI = KoreSymbolAPI
    { new :: Text -> LLVM KoreSymbolPtr
    , addArgument :: KoreSymbolPtr -> KoreSortPtr -> LLVM KoreSymbolPtr
    }

data KoreSortAPI = KoreSortAPI
    { new :: Text -> LLVM KoreSortPtr
    , addArgument :: KoreSortPtr -> KoreSortPtr -> LLVM KoreSortPtr
    , dump :: KoreSortPtr -> LLVM String
    }

data KorePatternAPI = KorePatternAPI
    { new :: Text -> LLVM KorePatternPtr
    , addArgument :: KorePatternPtr -> KorePatternPtr -> LLVM KorePatternPtr
    , fromSymbol :: KoreSymbolPtr -> LLVM KorePatternPtr
    , string :: KoreStringPatternAPI
    , token :: KoreTokenPatternAPI
    , dump :: KorePatternPtr -> LLVM String
    }

data API = API
    { pattern :: KorePatternAPI
    , symbol :: KoreSymbolAPI
    , sort :: KoreSortAPI
    , simplifyBool :: KorePatternPtr -> LLVM Bool
    }

newtype LLVM a = LLVM (ReaderT API IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

{- | Uses dlopen to load a .so/.dylib C library at runtime. For doucmentation of flags such as `RTL_LAZY`, consult e.g.
     https://man7.org/linux/man-pages/man3/dlopen.3.html
-}
withDLib :: FilePath -> (Linker.DL -> IO a) -> IO a
withDLib dlib = Linker.withDL dlib [Linker.RTLD_LAZY]

runLLVM :: FilePath -> LLVM a -> IO a
runLLVM fp m = withDLib fp $ flip runLLVMwithDL m


runLLVMwithDL :: Linker.DL -> LLVM a -> IO a
runLLVMwithDL dlib (LLVM m) = flip runReaderT dlib $ do
    pttrn <- do
        freePattern <- korePatternFreeFunPtr

        newCompositePattern <- koreCompositePatternNew
        let new name =
                liftIO $
                    C.withCString (Text.unpack name) $
                        newCompositePattern >=> newForeignPtr freePattern

        addArgumentCompositePattern <- koreCompositePatternAddArgument
        let addArgument parent child = liftIO $ do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentCompositePattern rawParent
                finalizeForeignPtr child
                pure parent


        string <- do
            newString <- koreStringPatternNew
            pure $ KoreStringPatternAPI $ \name -> liftIO $ C.withCString (Text.unpack name) $ newString >=> newForeignPtr freePattern

        token <- do
            newToken <- korePatternNewToken
            pure $ KoreTokenPatternAPI $ \name sort -> liftIO $ C.withCString (Text.unpack name) $ \rawName -> withForeignPtr sort $ \rawSort ->
                newToken rawName rawSort >>= newForeignPtr freePattern

        fromSymbol <- do
            compositePatternFromSymbol <- koreCompositePatternFromSymbol
            pure $ \sym -> liftIO $ withForeignPtr sym $ compositePatternFromSymbol >=> newForeignPtr freePattern


        dump <- do
            dump' <- korePatternDump
            pure $ \ptr -> liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dump' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str
        pure KorePatternAPI{new, addArgument, string, token, fromSymbol, dump}

    symbol <- do
        freeSymbol <- koreSymbolFreeFunPtr

        newSymbol <- koreSymbolNew
        let new name =
                liftIO $
                    C.withCString (Text.unpack name) $
                        newSymbol >=> newForeignPtr freeSymbol

        addArgumentSymbol <- koreSymbolAddFormalArgument
        let addArgument sym sort = liftIO $ do
                withForeignPtr sym $ \rawSym -> withForeignPtr sort $ addArgumentSymbol rawSym
                pure sym

        
        pure KoreSymbolAPI{new, addArgument}

    sort <- do
        freeSort <- koreSortFreeFunPtr

        newSort <- koreCompositeSortNew
        let new name =
                liftIO $
                    C.withCString (Text.unpack name) $
                        newSort >=> newForeignPtr freeSort

        addArgumentSort <- koreCompositeSortAddArgument
        let addArgument parent child = liftIO $ do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentSort rawParent
                pure parent


        dump <- do
            dump' <- koreSortDump
            pure $ \ptr -> liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dump' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str
        
        pure KoreSortAPI{new, addArgument, dump}

    simplifyBool <- do
        simplify <- koreSimplifyBool
        pure $ \pattern -> liftIO $ withForeignPtr pattern $ (fmap (==1)) <$> simplify

    liftIO $ runReaderT m $ API {pattern = pttrn, symbol, sort, simplifyBool}

ask :: LLVM API
ask = LLVM Reader.ask


marshallSymbol :: Symbol -> LLVM KoreSymbolPtr
marshallSymbol s = do
    kore <- ask
    sym <- kore.symbol.new s.name
    forM_ s.argSorts $ \sort -> kore.symbol.addArgument sym =<< marshallSort sort
    kore.symbol.addArgument sym =<< marshallSort s.resultSort

marshallSort :: Sort -> LLVM KoreSortPtr
marshallSort = \case
    SortApp name args -> do
        kore <- ask
        sort <- kore.sort.new name
        forM_ args $ \s -> kore.sort.addArgument sort =<< marshallSort s
        pure sort
    SortVar _ -> error "marshalling SortVar unsupported"

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm t = do
    kore <- ask
    case t of
        SymbolApplication symbol trms -> do
            trm <- kore.pattern.fromSymbol =<< marshallSymbol symbol
            forM_ trms $ \t' -> kore.pattern.addArgument trm =<< marshallTerm t'
            pure trm
        AndTerm l r -> do
            andSym <- kore.symbol.new "\\and"
            void $ kore.symbol.addArgument andSym =<< marshallSort (sortOfTerm l)
            trm <- kore.pattern.fromSymbol andSym
            void $ kore.pattern.addArgument trm =<< marshallTerm l 
            kore.pattern.addArgument trm =<< marshallTerm r
        DomainValue sort val -> 
            join $ kore.pattern.token.new <$> pure val <*> marshallSort sort
        Var _ -> error "marshalling Var unsupported"

