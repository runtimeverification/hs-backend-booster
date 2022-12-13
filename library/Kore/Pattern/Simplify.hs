{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Simplify (
    simplifyPattern,
    simplifyPredicate,
) where

import Kore.LLVM (simplifyBool)
import Kore.Pattern.Base
import Kore.Pattern.Util (sortOfTerm)
import System.Posix.DynamicLinker qualified as Linker

simplifyPattern :: Maybe Linker.DL -> Pattern -> Pattern
simplifyPattern Nothing pat = pat
simplifyPattern (Just dl) pat =
    if sortOfTerm pat.term == SortApp "bool" []
        then
            Pattern
                (simplifyBool dl pat.term)
                pat.constraints
        else pat

simplifyPredicate :: Predicate -> Predicate
simplifyPredicate = id -- FIXME
