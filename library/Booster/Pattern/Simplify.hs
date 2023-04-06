{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Simplify (
    simplifyPredicate,
    splitBoolPredicates,
    simplifyConcrete,
) where

import Booster.Definition.Base
import Booster.LLVM (simplifyBool, simplifyTerm)
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base
import Booster.Pattern.Util (isConcrete, sortOfTerm)

{- | We want to break apart predicates of type `Y1 andBool ... Yn` apart, in case some of the `Y`s are abstract
which prevents the original expression from being fed to the LLVM simplifyBool function
-}
splitBoolPredicates :: Predicate -> [Predicate]
splitBoolPredicates p@(Predicate t)
    | isConcrete t = [p]
    | otherwise = case t of
        AndBool ts -> concatMap (splitBoolPredicates . Predicate) ts
        other -> [Predicate other]

simplifyPredicate :: Maybe LLVM.API -> Predicate -> Predicate
simplifyPredicate (Just api) (Predicate t) 
    | isConcrete t = Predicate $ if simplifyBool api t
            then TrueBool
            else FalseBool
simplifyPredicate _ p = p

{- | traverses a term top-down, using a given LLVM dy.lib to simplify
 the concrete parts (leaving variables alone)
-}
simplifyConcrete :: Maybe LLVM.API -> KoreDefinition -> Term -> Term
simplifyConcrete Nothing _ trm = trm
simplifyConcrete (Just mApi) def trm = recurse trm
  where
    recurse :: Term -> Term
    -- recursion scheme for this?
    --     cata $ \case does not work here, would need helpers for TermF not Term
    --         t | isConcreteF t -> simplifyTerm dl def t (sortOfTerm t)
    --         other -> embed other\
    recurse t@(Term attributes _)
        | attributes.isEvaluated =
            t
        | isConcrete t =
            simplifyTerm mApi def t (sortOfTerm t)
        | otherwise =
            case t of
                var@Var{} ->
                    var -- nothing to do. Should have isEvaluated set
                dv@DomainValue{} ->
                    dv -- nothing to do. Should have isEvaluated set
                AndTerm t1 t2 ->
                    AndTerm (recurse t1) (recurse t2)
                SymbolApplication sym sorts args ->
                    SymbolApplication sym sorts (map recurse args)
                Injection sources target sub ->
                    Injection sources target $ recurse sub
