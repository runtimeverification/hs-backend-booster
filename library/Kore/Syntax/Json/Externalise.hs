{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Syntax.Json.Externalise (
    externalisePattern,
) where

import Data.Foldable ()
import Kore.Pattern.Base qualified as Internal
import Kore.Syntax.Json.Base qualified as Syntax

import Kore.Syntax.Json.Internalise (externaliseSort, sortOfTerm)

externalisePattern ::
    Internal.Pattern ->
    Syntax.KorePattern
externalisePattern Internal.Pattern{term = term, constraints} =
    -- need a sort for the predicates in external format
    let sort = externaliseSort $ sortOfTerm term
        term' = externaliseTerm term
        predicate = multiAnd sort $ map (externalisePredicate sort) constraints
     in Syntax.KJAnd{sort, first = term', second = predicate}
  where
    multiAnd :: Syntax.Sort -> [Syntax.KorePattern] -> Syntax.KorePattern
    multiAnd _ [] = error "multiAnd: empty"
    multiAnd sort ps = foldl1 (Syntax.KJAnd sort) ps

externaliseTerm :: Internal.Term -> Syntax.KorePattern
externaliseTerm = \case
    Internal.AndTerm sort first' second' ->
        Syntax.KJAnd
            (externaliseSort sort)
            (externaliseTerm first')
            (externaliseTerm second')
    Internal.SymbolApplication _sort sorts symName args ->
        Syntax.KJApp
            (symbolNameToId symName)
            (map externaliseSort sorts)
            (map externaliseTerm args)
    Internal.DomainValue sort txt ->
        Syntax.KJDV (externaliseSort sort) txt
    Internal.Var Internal.Variable{variableSort = iSort, variableName = iName} ->
        Syntax.KJEVar (varNameToId iName) (externaliseSort iSort)

externalisePredicate :: Syntax.Sort -> Internal.Predicate -> Syntax.KorePattern
externalisePredicate sort =
    let recursion = externalisePredicate sort
     in \case
            Internal.AndPredicate p1 p2 ->
                Syntax.KJAnd{sort, first = recursion p1, second = recursion p2}
            Internal.Bottom ->
                Syntax.KJBottom{sort}
            Internal.Ceil term ->
                Syntax.KJCeil
                    { argSort = externaliseSort $ sortOfTerm term
                    , sort
                    , arg = externaliseTerm term
                    }
            Internal.EqualsTerm termSort t1 t2 ->
                Syntax.KJEquals
                    { argSort = externaliseSort termSort
                    , sort
                    , first = externaliseTerm t1
                    , second = externaliseTerm t2
                    }
            Internal.EqualsPredicate p1 p2 ->
                Syntax.KJEquals{argSort = sort, sort, first = recursion p1, second = recursion p2}
            Internal.Exists varName p1 ->
                let varSort = Syntax.SortVar . Syntax.Id $ "Sort" <> varName
                 in Syntax.KJExists{sort, var = varNameToId varName, varSort, arg = recursion p1}
            Internal.Forall varName p1 ->
                let varSort = Syntax.SortVar . sortNameToId $ "Sort" <> varName
                 in Syntax.KJForall{sort, var = varNameToId varName, varSort, arg = recursion p1}
            Internal.Iff p1 p2 ->
                Syntax.KJIff{sort, first = recursion p1, second = recursion p2}
            Internal.Implies p1 p2 ->
                Syntax.KJImplies{sort, first = recursion p1, second = recursion p2}
            Internal.In termSort t1 t2 ->
                Syntax.KJIn
                    { argSort = externaliseSort termSort
                    , sort
                    , first = externaliseTerm t1
                    , second = externaliseTerm t2
                    }
            Internal.Not p1 ->
                Syntax.KJNot{sort, arg = recursion p1}
            Internal.Or p1 p2 ->
                Syntax.KJOr{sort, first = recursion p1, second = recursion p2}
            Internal.Top ->
                Syntax.KJTop{sort}

varNameToId :: Internal.VarName -> Syntax.Id
varNameToId = Syntax.Id

sortNameToId :: Internal.SortName -> Syntax.Id
sortNameToId = Syntax.Id

symbolNameToId :: Internal.SymbolName -> Syntax.Id
symbolNameToId = Syntax.Id
