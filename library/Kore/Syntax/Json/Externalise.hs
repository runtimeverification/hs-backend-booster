{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Syntax.Json.Externalise (
    externalisePattern,
    externaliseSort,
) where

import Data.Foldable ()

import Kore.Pattern.Base qualified as Internal
import Kore.Pattern.Util (sortOfTerm)
import Kore.Syntax.Json.Base qualified as Syntax

{- | Converts an internal pattern to a pair of term and predicate in
 external format. The predicate is 'And'ed to avoid leaking
 Json format internals to the caller.
-}
externalisePattern ::
    Internal.Pattern ->
    (Syntax.KorePattern, Maybe Syntax.KorePattern)
externalisePattern Internal.Pattern{term = term, constraints} =
    -- need a sort for the predicates in external format
    let sort = externaliseSort $ sortOfTerm term
        predicate =
            if null constraints
                then Nothing
                else Just $ multiAnd sort $ map (externalisePredicate sort) constraints
     in (externaliseTerm term, predicate)
  where
    multiAnd :: Syntax.Sort -> [Syntax.KorePattern] -> Syntax.KorePattern
    multiAnd _ [] = error "multiAnd: empty"
    multiAnd sort ps = foldl1 (Syntax.KJAnd sort) ps

-- TODO: should KorePattern be the only type with an actual Unparse instance?
externaliseTerm :: Internal.Term -> Syntax.KorePattern
externaliseTerm = \case
    Internal.AndTerm first' second' ->
        Syntax.KJAnd
            (externaliseSort $ sortOfTerm second')
            (externaliseTerm first')
            (externaliseTerm second')
    Internal.SymbolApplication symbol sorts args ->
        Syntax.KJApp
            (symbolNameToId symbol.name)
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
            Internal.EqualsTerm t1 t2 ->
                Syntax.KJEquals
                    { argSort = externaliseSort $ sortOfTerm t1
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
            Internal.In t1 t2 ->
                Syntax.KJIn
                    { argSort = externaliseSort $ sortOfTerm t1
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

-- | converts an internal sort to an external one
externaliseSort :: Internal.Sort -> Syntax.Sort
externaliseSort (Internal.SortApp name args) =
    Syntax.SortApp (sortNameToId name) (map externaliseSort args)
externaliseSort (Internal.SortVar name) =
    Syntax.SortVar $ sortNameToId name
