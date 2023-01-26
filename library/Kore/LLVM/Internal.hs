{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Kore.LLVM.Internal (API (..), KorePatternAPI (..), runLLVM, withDLib, mkAPI, ask, marshallTerm, marshallSort, freeLlvmMemory) where

import Control.Monad (foldM, forM_, void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (packCStringLen)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Set (toList, fromList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Hashable (Hashable, hash)
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
    , cache :: IORef (HashMap Sort KoreSortPtr)
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
    , freeAllMemory :: IO ()
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
    let newPattern name =
            {-# SCC "LLVM.pattern.new" #-}
            liftIO $
                C.withCString (Text.unpack name) $
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
                C.withCString (Text.unpack name) $
                    newSymbol' >=> newForeignPtr freeSymbol

    addArgumentSymbol' <- koreSymbolAddFormalArgument
    let addArgumentSymbol sym sort =
            liftIO $
                {-# SCC "LLVM.symbol.addArgument" #-}
                do
                    withForeignPtr sym $ \rawSym -> withForeignPtr sort $ addArgumentSymbol' rawSym
                    pure sym

    let symbol = KoreSymbolAPI{new = newSymbol, addArgument = addArgumentSymbol}

    freeSort <- {-# SCC "LLVM.sort.free" #-} koreSortFreeFunPtr

    newSort' <- koreCompositeSortNew
    let newSort name =
            {-# SCC "LLVM.sort.new" #-}
            liftIO $
                C.withCString (Text.unpack name) $
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


    freeAllMemory <- kllvmFreeAllMemory

    pure API{patt, symbol, sort, simplifyBool, simplify, freeAllMemory}

ask :: LLVM API
ask = LLVM Reader.ask

marshallSymbol :: Symbol -> [Sort] -> LLVM KoreSymbolPtr
marshallSymbol sym sorts = do
    kore <- ask
    sym' <- liftIO $ kore.symbol.new sym.name
    liftIO $ Text.appendFile "./trace.txt" ("kore_symbol* " <> hshNm "sym" sym <> " = kore_symbol_new(\"" <> sym.name <> "\");\n")
    res <- foldM (\symbol sort' -> do
        sort <- marshallSort sort'
        -- liftIO $ modifyIORef' kore.sort.cache $ HM.delete sort'
        liftIO $ Text.appendFile "./trace.txt" ("kore_symbol_add_formal_argument(" <> hshNm "sym" sym <> ", " <> hshNm "srt" sort' <> ");\n")
        liftIO $ kore.symbol.addArgument symbol sort) sym' sorts
    -- liftIO $ modifyIORef' kore.symbol.cache $ HM.insert (sym,sorts) res
    pure res

hshNm :: Hashable o => Text -> o -> Text
hshNm nm o = let h = hash o in if h < 0 then nm <> "_m" <> (Text.pack $ show $ abs h) else nm <> "_" <> (Text.pack $ show h)

marshallSort :: Sort -> LLVM KoreSortPtr
marshallSort = \case
    s@(SortApp name args) -> do
        kore <- ask
        cache <- liftIO $ readIORef kore.sort.cache
        case HM.lookup s cache of
            Just ptr -> pure ptr
            Nothing -> do
                sort <- liftIO $ kore.sort.new name
                liftIO $ Text.appendFile "./trace.txt" ("kore_sort* " <> hshNm "srt" s <> " = kore_composite_sort_new(\"" <> name <> "\");\n")
                forM_ args $ \s' -> do
                    sort' <- marshallSort s'
                    liftIO $ Text.appendFile "./trace.txt" ("kore_composite_sort_add_argument(" <>  hshNm "srt" s <> ", " <> hshNm "srt" s' <> ");\n")
                    liftIO $ kore.sort.addArgument sort sort'
                liftIO $ modifyIORef' kore.sort.cache $ HM.insert s sort
                pure sort
    SortVar varName -> error $ "marshalling SortVar " <> show varName <> " unsupported"

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm t = do
    kore <- ask
    case t of
        SymbolApplication symbol sorts trms -> do
            liftIO $ Text.appendFile "./trace.txt" ("// begin " <> hshNm "trm" t <> "\n")
            trm <- liftIO . kore.patt.fromSymbol =<< marshallSymbol symbol sorts
            liftIO $ Text.appendFile "./trace.txt" ("kore_pattern* " <> hshNm "pat" t <> " = kore_composite_pattern_from_symbol(" <> hshNm "sym" symbol <> ");\n")
            forM_ trms $ \childT' -> do
                childT <- marshallTerm childT'
                ret <- liftIO $ kore.patt.addArgument trm childT
                liftIO $ Text.appendFile "./trace.txt" ("kore_composite_pattern_add_argument(" <> hshNm "pat" t <> ", " <> hshNm "pat" childT' <> ");\n")
                pure ret
            liftIO $ Text.appendFile "./trace.txt" ("// end " <> hshNm "trm" t <> "\n\n")
            pure trm
        AndTerm l r -> do
            liftIO $ Text.appendFile "./trace.txt" ("// begin " <> hshNm "trm" t <> "\n")
            andSym <- liftIO $ kore.symbol.new "\\and"
            liftIO $ Text.appendFile "./trace.txt" ("kore_symbol* " <> hshNm "and" t <> " = kore_symbol_new(\"\\and\");\n")
            void $ liftIO . kore.symbol.addArgument andSym =<< marshallSort (sortOfTerm l)
            liftIO $ Text.appendFile "./trace.txt" ("kore_symbol_add_formal_argument(" <> hshNm "and" t <> ", " <> hshNm "srt" (sortOfTerm l) <> ");\n")
            andTrm <- liftIO $ kore.patt.fromSymbol andSym
            liftIO $ Text.appendFile "./trace.txt" ("kore_pattern* " <> hshNm "pat" t <> " = kore_composite_pattern_from_symbol(" <> hshNm "and" t <> ");\n")
            void $ liftIO . kore.patt.addArgument andTrm =<< marshallTerm l
            liftIO $ Text.appendFile "./trace.txt" ("kore_composite_pattern_add_argument(" <> hshNm "pat" t <> ", " <> hshNm "pat" l <> ");\n")
            trm <- liftIO . kore.patt.addArgument andTrm =<< marshallTerm r
            liftIO $ Text.appendFile "./trace.txt" ("kore_composite_pattern_add_argument(" <> hshNm "pat" t <> ", " <> hshNm "pat" r <> ");\n")
            liftIO $ Text.appendFile "./trace.txt" ("// end " <> hshNm "trm" t <> "\n\n")
            pure trm
        DomainValue sort' val -> do
            liftIO $ Text.appendFile "./trace.txt" ("// begin " <> hshNm "trm" t <> "\n")
            sort <- marshallSort sort'
            liftIO $ Text.appendFile "./trace.txt" ("kore_pattern* " <> hshNm "pat" t <> " = kore_pattern_new_token(\"" <> val <> "\", " <> hshNm "srt" sort' <> ");\n")
            liftIO $ Text.appendFile "./trace.txt" ("// end " <> hshNm "trm" t <> "\n\n")
            trm <- liftIO $ kore.patt.token.new val sort
            pure trm
        Var varName -> error $ "marshalling Var " <> show varName <> " unsupported"


freeLlvmMemory :: API -> IO ()
freeLlvmMemory api = do
    cache <- readIORef api.sort.cache
    writeIORef api.sort.cache mempty
    forM_ (toList $ fromList $ HM.elems cache) finalizeForeignPtr
    api.freeAllMemory
    Text.appendFile "./trace.txt" ("\n\n\nkllvm_free_all_memory();\n\n\n")


