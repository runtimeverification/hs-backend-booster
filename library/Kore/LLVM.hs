module Kore.LLVM (simplifyBool) where

import Kore.LLVM.Internal qualified as Internal
import Kore.Pattern.Base

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker qualified as Linker

simplifyBool :: Linker.DL -> Term -> Term
simplifyBool dl trm = unsafePerformIO $ Internal.runLLVMwithDL dl $ do
    kore <- Internal.ask
    res <- Internal.marshallTerm trm >>= kore.simplifyBool
    pure $
        DomainValue (SortApp "bool" []) $
            if res
                then "true"
                else "false"
