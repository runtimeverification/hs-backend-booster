{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Util (
    sortOfTerm,
    sortOfTermOrPredicate,
    retractPattern,
    substituteInTerm,
    substituteInPredicate,
    freeVariables,
    isConstructorSymbol,
    isSortInjectionSymbol,
    isDefinedSymbol,
    checkSymbolIsAc,
    checkTermSymbols,
) where

import Data.Foldable (fold)
import Data.Functor.Foldable (Corecursive (embed), cata)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Kore.Definition.Attributes.Base (SymbolAttributes (..), SymbolType (..))
import Kore.Pattern.Base

-- | Returns the sort of a term
sortOfTerm :: Term -> Sort
sortOfTerm (AndTerm _ child) = sortOfTerm child
sortOfTerm (SymbolApplication symbol _) = symbol.resultSort
sortOfTerm (DomainValue sort _) = sort
sortOfTerm (Var Variable{variableSort}) = variableSort

sortOfTermOrPredicate :: TermOrPredicate -> Maybe Sort
sortOfTermOrPredicate (TermAndPredicate Pattern{term}) = Just (sortOfTerm term)
sortOfTermOrPredicate (APredicate _) = Nothing

retractPattern :: TermOrPredicate -> Maybe Pattern
retractPattern (TermAndPredicate patt) = Just patt
retractPattern _ = Nothing

substituteInTerm :: Map Variable Term -> Term -> Term
substituteInTerm substitution = cata $ \case
    VarF v -> fromMaybe (Var v) (Map.lookup v substitution)
    other -> embed other

substituteInPredicate :: Map Variable Term -> Predicate -> Predicate
substituteInPredicate substitution = cata $ \case
    EqualsTermF t1 t2 ->
        EqualsTerm (substituteInTerm substitution t1) (substituteInTerm substitution t2)
    other -> embed other

freeVariables :: Term -> Set Variable
freeVariables = cata $ \case
    VarF var -> Set.singleton var
    other -> fold other

isConstructorSymbol :: Symbol -> Bool
isConstructorSymbol symbol =
    case symbol.attributes.symbolType of
        Constructor -> True
        _ -> False

isSortInjectionSymbol :: Symbol -> Bool
isSortInjectionSymbol symbol =
    case symbol.attributes.symbolType of
        SortInjection -> True
        _ -> False

isDefinedSymbol :: Symbol -> Bool
isDefinedSymbol symbol =
    case symbol.attributes.symbolType of
        Constructor -> True
        TotalFunction -> True
        SortInjection -> True
        PartialFunction -> False

checkSymbolIsAc :: Symbol -> Bool
checkSymbolIsAc symbol =
    symbol.attributes.isAssoc || symbol.attributes.isIdem

checkTermSymbols :: (Symbol -> Bool) -> Term -> Bool
checkTermSymbols check = cata $ \case
    SymbolApplicationF symbol ts -> check symbol && foldr (&&) True ts
    other -> foldr (&&) True other
