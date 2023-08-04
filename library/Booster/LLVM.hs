module Booster.LLVM (simplifyBool, simplifyTerm) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Get
import Data.ByteString qualified as BS (fromStrict)
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.IO.Unsafe (unsafePerformIO)

import Booster.Definition.Base
import Booster.LLVM.Internal qualified as Internal
import Booster.Pattern.Base
import Booster.Pattern.Binary
import Booster.Pattern.Util
import Booster.Trace qualified as Trace

simplifyBool :: Internal.API -> Term -> Bool
simplifyBool api trm = unsafePerformIO $ Internal.runLLVM api $ do
    kore <- Internal.ask
    trmPtr <- Trace.timeIO "LLVM.simplifyBool.marshallTerm" (Internal.marshallTerm trm)

    Trace.traceIO $ Internal.LlvmVar (Internal.somePtr trmPtr) trm

    liftIO $ kore.simplifyBool trmPtr

simplifyTerm :: Internal.API -> KoreDefinition -> Term -> Sort -> Term
simplifyTerm api def trm sort = unsafePerformIO . Internal.runLLVM api $ do
    kore <- Internal.ask
    -- initialize KLLVM
    liftIO kore.initKllvm
    trmPtr <- Trace.timeIO "LLVM.simplifyTerm.marshallTerm" $ Internal.marshallTerm trm
    sortPtr <- Trace.timeIO "LLVM.simplifyTerm.marshallSort" $ Internal.marshallSort sort
    binary <- liftIO $ kore.simplify trmPtr sortPtr
    Trace.traceIO $ Internal.LlvmVar (Internal.somePtr trmPtr) trm
    let decodedTerm = runGet (decodeTerm def) (BS.fromStrict binary)
    -- strip away the custom injection added by the LLVM backend
    finalTerm <- Trace.timeIO "LLVM.simplifyTerm.decodeTerm" $ case decodedTerm of
        Injection newSort (SortApp "SortKItem" _) result
            | newSort == sort ->
                pure result
            | Set.member (sortName newSort) subsorts ->
                pure $ Injection newSort sort result
        someTerm
            | sortOfTerm someTerm == sort ->
                pure someTerm
        other -> do
            liftIO . putStrLn $
                "[Error] Unexpected sort after LLVM simplification: "
                    <> show other
                    <> " Expected sort "
                    <> show sort
            pure trm
    -- call KLLVM garbage collector
    liftIO kore.freeAllMemory
    pure finalTerm
  where
    sortName (SortApp name _) = name
    sortName (SortVar name) = name
    subsorts = maybe Set.empty snd $ Map.lookup (sortName sort) def.sorts
