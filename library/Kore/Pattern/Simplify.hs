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
import Kore.Pattern.Util (isConcrete, sortOfTerm)
import System.Posix.DynamicLinker qualified as Linker

simplifyPattern :: Maybe Linker.DL -> Pattern -> Pattern
simplifyPattern Nothing pat = pat
simplifyPattern (Just dl) pat =
    if isConcrete pat.term && sortOfTerm pat.term == bool
        then
            Pattern
                (DomainValue bool $
                    if simplifyBool dl pat.term
                    then "true"
                    else "false")
                pat.constraints
        else pat
    where
        bool = SortApp "bool" []

simplifyPredicate :: Maybe Linker.DL -> Predicate -> Predicate
simplifyPredicate dl = \case
    AndPredicate l r -> AndPredicate (simplifyPredicate dl l) (simplifyPredicate dl r)
    Bottom -> Bottom
    p@(Ceil _) -> p
    p@(EqualsTerm l r) ->
        if sortOfTerm l == SortApp "bool" [] 
            then case dl of
                Nothing -> p
                Just dlib -> 
                    if simplifyBool dlib l == simplifyBool dlib r
                        then Top
                        else Bottom
            else p
    EqualsPredicate l r -> EqualsPredicate (simplifyPredicate dl l) (simplifyPredicate dl r)
    p@(Exists _ _) -> p
    p@(Forall _ _) -> p
    Iff l r -> Iff (simplifyPredicate dl l) (simplifyPredicate dl r)
    Implies l r -> Implies (simplifyPredicate dl l) (simplifyPredicate dl r)
    p@(In _ _) -> p
    Not p -> case simplifyPredicate dl p of
        Top -> Bottom
        Bottom -> Top
        p' -> p'
    Or l r -> Or (simplifyPredicate dl l) (simplifyPredicate dl r)
    Top -> Top