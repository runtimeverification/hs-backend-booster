module Booster.LLVM (simplifyBool, simplifyTerm) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Get
import Data.Binary
import Data.ByteString (fromStrict)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Prettyprinter
import System.IO.Unsafe (unsafePerformIO)

import Booster.Definition.Base
import Booster.LLVM.Internal qualified as Internal
import Booster.Pattern.Base
import Booster.Pattern.Binary
import Booster.Pattern.Util
import Booster.Prettyprinter
import Booster.Trace qualified as Trace

data LlvmTerm
    = LlvmBool String
    | LlvmTerm String String

instance Trace.CustomUserEvent LlvmTerm where
    encodeUserEvent (LlvmBool b) = put @String "Bool" >> put b
    encodeUserEvent (LlvmTerm t s) = put @String "Term" >> put t >> put s
    decodeUserEvent = do
        tag <- getByteString 4
        case tag of
            "Bool" -> LlvmBool <$> get
            "Term" -> LlvmTerm <$> get <*> get
            other -> error $ "decodeUserEvent @LlvmTerm: Unexpected tag " <> show other
    userEventTag _ = "TERM "
    eventType _ = Trace.LlvmData

toText :: Pretty a => a -> String
toText = renderDefault . pretty

simplifyBool :: Internal.API -> Term -> Bool
simplifyBool api trm = unsafePerformIO $ Internal.runLLVM api $ do
    Trace.traceIO $ LlvmBool (toText trm)
    kore <- Internal.ask
    trmPtr <- Trace.timeIO "LLVM.simplifyBool.marshallTerm" (Internal.marshallTerm trm)

    Trace.traceIO $ Internal.LlvmVar (Internal.somePtr trmPtr) trm

    Trace.timeIO "LLVM.simplifyBool.kore" $ liftIO $ kore.simplifyBool trmPtr

simplifyTerm :: Internal.API -> KoreDefinition -> Term -> Sort -> Term
simplifyTerm api def trm sort = unsafePerformIO $ Internal.runLLVM api $ do
    Trace.traceIO $ LlvmTerm (toText trm) (toText sort)
    kore <- Internal.ask
    trmPtr <- Trace.timeIO "LLVM.simplifyTerm.marshallTerm" $ Internal.marshallTerm trm
    sortPtr <- Trace.timeIO "LLVM.simplifyTerm.marshallSort" $ Internal.marshallSort sort
    binary <- liftIO $ kore.simplify trmPtr sortPtr
    Trace.traceIO $ Internal.LlvmVar (Internal.somePtr trmPtr) trm
    -- strip away the custom injection added by the LLVM backend
    Trace.timeIO "LLVM.simplifyTerm.decodeTerm" $ case runGet (decodeTerm def) (fromStrict binary) of
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
  where
    sortName (SortApp name _) = name
    sortName (SortVar name) = name
    subsorts = maybe Set.empty snd $ Map.lookup (sortName sort) def.sorts
