module Kore.LLVM (simplifyBool, simplifyTerm) where

import Kore.LLVM.Internal qualified as Internal
import Kore.Pattern.Base

import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Get
import Data.ByteString (fromStrict, toStrict)
import Data.ByteString qualified as BS
import Kore.Definition.Base
import Kore.Pattern.Binary
import Kore.Pattern.Util
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary.Put (runPut)

simplifyBool :: Internal.API -> Term -> Bool
simplifyBool api trm = unsafePerformIO $ Internal.runLLVM api $ do
    kore <- Internal.ask
    Internal.marshallTerm trm >>= liftIO . kore.simplifyBool

simplifyTerm :: Internal.API -> KoreDefinition -> Term -> Sort -> Term
simplifyTerm api def trm sort = unsafePerformIO $ Internal.runLLVM api $ do
    kore <- Internal.ask
    sortPtr <- Internal.marshallSort sort
    -- trmPtr <- Internal.marshallTerm trm
    -- binary <- liftIO $ kore.simplify trmPtr sortPtr

    let trmStr = toStrict $ runPut $ encodeMagicHeaderAndVersion (Version 1 1 0) >> encodeTerm trm
    liftIO $ BS.writeFile "dump.bin" trmStr
    binary <- liftIO $ kore.simplifyBinary trmStr sortPtr
    -- strip away the custom injection added by the LLVM backend
    case runGet (decodeTerm def) (fromStrict binary) of
        Injection origSort (SortApp "SortKItem" _) result
            | origSort == sort ->
                pure result
        someTerm
            | sortOfTerm someTerm == sort ->
                pure someTerm
        other ->
            error $ "Unexpected sort after LLVM simplification: " <> show other
