{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Kore.LLVM.Internal (
    API (..),
    KorePatternAPI (..),
    runLLVM,
    withDLib,
    mkAPI,
    ask,
    marshallTerm,
    marshallSort,
    -- testing only
    KoreStringPatternAPI (..),
    KoreSymbolAPI (..),
    KoreSortAPI (..),
) where

import Control.Monad (foldM, forM_, void, (>=>))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign qualified
import Foreign.C qualified as C
import Foreign.C.Types (CSize (..))
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Kore.LLVM.TH (dynamicBindings)
import Kore.Pattern.Base
import Kore.Pattern.Binary qualified as Binary
import Kore.Pattern.Util (sortOfTerm)
import Kore.Trace (CustomUserEvent (LlvmCall, LlvmVar), toPtr, toPtrList)
import Kore.Trace qualified as Trace
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
    { new :: ByteString -> IO KorePatternPtr
    }

newtype KoreTokenPatternAPI = KoreTokenPatternAPI
    { new :: ByteString -> KoreSortPtr -> IO KorePatternPtr
    }

data KoreSymbolAPI = KoreSymbolAPI
    { new :: ByteString -> IO KoreSymbolPtr
    , addArgument :: KoreSymbolPtr -> KoreSortPtr -> IO KoreSymbolPtr
    , cache :: IORef (HashMap (Symbol, [Sort]) KoreSymbolPtr)
    }

data KoreSortAPI = KoreSortAPI
    { new :: ByteString -> IO KoreSortPtr
    , addArgument :: KoreSortPtr -> KoreSortPtr -> IO KoreSortPtr
    , dump :: KoreSortPtr -> IO String
    , cache :: IORef (HashMap Sort KoreSortPtr)
    }

data KorePatternAPI = KorePatternAPI
    { new :: ByteString -> IO KorePatternPtr
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
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask)

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
    let newPattern name =
            {-# SCC "LLVM.pattern.new" #-}
            liftIO $
                BS.useAsCString name $
                    newCompositePattern >=> newForeignPtr freePattern

    addArgumentCompositePattern <- koreCompositePatternAddArgument
    let addArgumentPattern parent child =
            liftIO $
                {-# SCC "LLVM.pattern.addArgument" #-}
                do
                    withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentCompositePattern rawParent
                    finalizeForeignPtr child
                    pure parent

    newString <- koreStringPatternNewWithLen
    let string = KoreStringPatternAPI $ \name ->
            {-# SCC "LLVM.pattern.string" #-}
            liftIO $ BS.useAsCStringLen name $ \(rawStr, len) ->
                newString rawStr (fromIntegral len) >>= newForeignPtr freePattern

    newToken <- korePatternNewTokenWithLen
    let token = KoreTokenPatternAPI $ \name sort ->
            {-# SCC "LLVM.pattern.token" #-}
            liftIO $ BS.useAsCStringLen name $ \(rawName, len) ->
                withForeignPtr sort $
                    newToken rawName (fromIntegral len) >=> newForeignPtr freePattern

    compositePatternFromSymbol <- koreCompositePatternFromSymbol
    let fromSymbol sym = {-# SCC "LLVM.pattern.fromSymbol" #-} liftIO $ withForeignPtr sym $ compositePatternFromSymbol >=> newForeignPtr freePattern

    dumpPattern' <- korePatternDump
    let dumpPattern ptr =
            {-# SCC "LLVM.pattern.dump" #-}
            liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dumpPattern' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

    let patt = KorePatternAPI{new = newPattern, addArgument = addArgumentPattern, string, token, fromSymbol, dump = dumpPattern}

    freeSymbol <- {-# SCC "LLVM.symbol.free" #-} koreSymbolFreeFunPtr

    newSymbol' <- koreSymbolNew
    let newSymbol name =
            {-# SCC "LLVM.symbol.new" #-}
            liftIO $
                BS.useAsCString name $
                    newSymbol' >=> newForeignPtr freeSymbol

    addArgumentSymbol' <- koreSymbolAddFormalArgument
    let addArgumentSymbol sym sort =
            liftIO $
                {-# SCC "LLVM.symbol.addArgument" #-}
                do
                    withForeignPtr sym $ \rawSym -> withForeignPtr sort $ addArgumentSymbol' rawSym
                    pure sym

    symbolCache <- liftIO $ newIORef mempty

    let symbol = KoreSymbolAPI{new = newSymbol, addArgument = addArgumentSymbol, cache = symbolCache}

    freeSort <- {-# SCC "LLVM.sort.free" #-} koreSortFreeFunPtr

    newSort' <- koreCompositeSortNew
    let newSort name =
            {-# SCC "LLVM.sort.new" #-}
            liftIO $
                BS.useAsCString name $
                    newSort' >=> newForeignPtr freeSort

    addArgumentSort' <- koreCompositeSortAddArgument
    let addArgumentSort parent child =
            liftIO $
                {-# SCC "LLVM.sort.addArgument" #-}
                do
                    withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentSort' rawParent
                    pure parent

    dumpSort' <- koreSortDump
    let dumpSort ptr =
            {-# SCC "LLVM.sort.dump" #-}
            liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dumpSort' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

    sortCache <- liftIO $ newIORef mempty

    let sort = KoreSortAPI{new = newSort, addArgument = addArgumentSort, dump = dumpSort, cache = sortCache}

    simplifyBool' <- koreSimplifyBool
    let simplifyBool p = {-# SCC "LLVM.simplifyBool" #-} Trace.timeIO "LLVM.simplifyBool" $ liftIO $ withForeignPtr p $ fmap (== 1) <$> simplifyBool'

    simplify' <- koreSimplify
    let simplify pat srt =
            {-# SCC "LLVM.simplify" #-}
            Trace.timeIO "LLVM.simplify" $
                liftIO $
                    withForeignPtr pat $ \patPtr ->
                        withForeignPtr srt $ \sortPtr ->
                            alloca $ \lenPtr ->
                                alloca $ \strPtr -> do
                                    simplify' patPtr sortPtr strPtr lenPtr
                                    len <- fromIntegral <$> peek lenPtr
                                    cstr <- peek strPtr
                                    BS.packCStringLen (cstr, len)

    pure API{patt, symbol, sort, simplifyBool, simplify}

ask :: LLVM API
ask = LLVM Reader.ask

marshallSymbol :: Symbol -> [Sort] -> LLVM KoreSymbolPtr
marshallSymbol sym sorts =
    do
        kore <- ask
        cache <- liftIO $ readIORef kore.symbol.cache
        case HM.lookup (sym, sorts) cache of
            Just ptr -> pure ptr
            Nothing -> do
                sym' <- liftIO $ kore.symbol.new sym.name
                Trace.traceIO $
                    LlvmCall
                        { ret = Just $ toPtr $ Binary.BSymbol sym.name sorts
                        , call = "kore_symbol_new"
                        , args = toPtrList [Binary.BString sym.name]
                        }
                Trace.traceIO $ LlvmVar $ Binary.BSymbol sym.name sym.argSorts
                liftIO $ modifyIORef' kore.symbol.cache $ HM.insert (sym, sorts) sym'
                foldM
                    ( \symbol sort -> do
                        sortPtr <- marshallSort sort
                        symPtr <- liftIO $ kore.symbol.addArgument symbol sortPtr
                        Trace.traceIO $
                            LlvmCall
                                { ret = Nothing
                                , call = "kore_symbol_add_formal_argument"
                                , args = toPtrList [Binary.BSymbol sym.name sorts, Binary.BSort sort]
                                }
                        pure symPtr
                    )
                    sym'
                    sorts

marshallSort :: Sort -> LLVM KoreSortPtr
marshallSort s =
    case s of
        SortApp name args -> do
            kore <- ask
            cache <- liftIO $ readIORef kore.sort.cache
            case HM.lookup s cache of
                Just ptr -> pure ptr
                Nothing -> do
                    sort <- liftIO $ kore.sort.new name
                    Trace.traceIO $
                        LlvmCall
                            { ret = Just $ toPtr $ Binary.BSort s
                            , call = "kore_composite_sort_new"
                            , args = toPtrList [Binary.BString name]
                            }
                    Trace.traceIO $ LlvmVar $ Binary.BSort s
                    forM_ args $ \s' -> do
                        sort' <- marshallSort s'
                        void $ liftIO $ kore.sort.addArgument sort sort'
                        Trace.traceIO $
                            LlvmCall
                                { ret = Nothing
                                , call = "kore_composite_sort_add_argument"
                                , args = toPtrList [Binary.BSort s, Binary.BSort s']
                                }
                    liftIO $ modifyIORef' kore.sort.cache $ HM.insert s sort
                    pure sort
        SortVar varName -> error $ "marshalling SortVar " <> show varName <> " unsupported"

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm t =
    do
        kore <- ask
        case t of
            SymbolApplication symbol sorts trms -> do
                trm <- liftIO . kore.patt.fromSymbol =<< marshallSymbol symbol sorts
                Trace.traceIO $
                    LlvmCall
                        { ret = Just $ toPtr $ Binary.BTerm t
                        , call = "kore_composite_pattern_from_symbol"
                        , args = toPtrList [Binary.BSymbol symbol.name sorts]
                        }
                forM_ trms $ \childT' -> do
                    childT <- marshallTerm childT'
                    ret <- liftIO $ kore.patt.addArgument trm childT
                    Trace.traceIO $
                        LlvmCall
                            { ret = Nothing
                            , call = "kore_composite_pattern_add_argument"
                            , args = toPtrList [Binary.BTerm t, Binary.BTerm childT']
                            }
                    pure ret
                pure trm
            AndTerm l r -> do
                andTrm <- liftIO . kore.patt.fromSymbol =<< marshallSymbol andSymbol [sortOfTerm l]
                Trace.traceIO $
                    LlvmCall
                        { ret = Just $ toPtr $ Binary.BTerm t
                        , call = "kore_composite_pattern_from_symbol"
                        , args = toPtrList [Binary.BSymbol andSymbol.name [sortOfTerm l]]
                        }
                void $ liftIO . kore.patt.addArgument andTrm =<< marshallTerm l
                Trace.traceIO $
                    LlvmCall
                        { ret = Nothing
                        , call = "kore_composite_pattern_add_argument"
                        , args = toPtrList [Binary.BTerm t, Binary.BTerm l]
                        }
                trm <- liftIO . kore.patt.addArgument andTrm =<< marshallTerm r
                Trace.traceIO $
                    LlvmCall
                        { ret = Nothing
                        , call = "kore_composite_pattern_add_argument"
                        , args = toPtrList [Binary.BTerm t, Binary.BTerm r]
                        }
                pure trm
            DomainValue sort' val -> do
                sort <- marshallSort sort'
                trm <- liftIO $ kore.patt.token.new val sort
                Trace.traceIO $
                    LlvmCall
                        { ret = Just $ toPtr $ Binary.BTerm t
                        , call = "kore_pattern_new_token"
                        , args = toPtrList [Binary.BString val, Binary.BSort sort']
                        }
                pure trm
            Var varName -> error $ "marshalling Var " <> show varName <> " unsupported"
            Injection source target child -> do
                inj <- liftIO . kore.patt.fromSymbol =<< marshallSymbol injectionSymbol [source, target]
                Trace.traceIO $
                    LlvmCall
                        { ret = Just $ toPtr $ Binary.BTerm t
                        , call = "kore_composite_pattern_from_symbol"
                        , args = toPtrList [Binary.BSymbol injectionSymbol.name [source, target]]
                        }
                trm <- liftIO . kore.patt.addArgument inj =<< marshallTerm child
                Trace.traceIO $
                    LlvmCall
                        { ret = Nothing
                        , call = "kore_composite_pattern_add_argument"
                        , args = toPtrList [Binary.BTerm t, Binary.BTerm child]
                        }
                pure trm
