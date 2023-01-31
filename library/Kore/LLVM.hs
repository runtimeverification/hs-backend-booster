module Kore.LLVM (simplifyBool, simplifyTerm) where

import Kore.LLVM.Internal qualified as Internal
import Kore.Pattern.Base

import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Get
import Data.ByteString (fromStrict)
import Kore.Definition.Base
import Kore.Pattern.Binary
import Kore.Pattern.Util
import System.IO.Unsafe (unsafePerformIO)
import Data.Tree

simplifyBool :: Internal.API -> Term -> Bool
simplifyBool api trm = unsafePerformIO $ Internal.runLLVM api $ do
    kore <- Internal.ask
    ptrTree <- Internal.marshallTerm trm 
    res <- liftIO $ kore.simplifyBool $ rootLabel ptrTree
    liftIO $ Internal.finalizeKorePatternPtrTree ptrTree
    Internal.printStats
    liftIO $ kore.gc
    pure res

simplifyTerm :: Internal.API -> KoreDefinition -> Term -> Sort -> Term
simplifyTerm api def trm sort = unsafePerformIO $ Internal.runLLVM api $ do
    kore <- Internal.ask
    trmPtrTree <- Internal.marshallTerm trm
    sortPtr <- Internal.marshallSort sort
    binary <- liftIO $ kore.simplify (rootLabel trmPtrTree) sortPtr
    liftIO $ Internal.finalizeKorePatternPtrTree trmPtrTree
    Internal.printStats
    liftIO $ kore.gc
    -- strip away the custom injection added by the LLVM backend
    case runGet (decodeKorePattern def) (fromStrict binary) of
        Injection origSort (SortApp "SortKItem" _) result
            | origSort == sort ->
                pure result
        someTerm
            | sortOfTerm someTerm == sort ->
                pure someTerm
        other ->
            error $ "Unexpected sort after LLVM simplification: " <> show other
