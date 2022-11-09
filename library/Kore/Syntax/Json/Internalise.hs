{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Syntax.Json.Internalise (
    internalisePattern,
    PatternError (..),
    checkSort,
    SortError (..),
) where

import Control.Monad
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.List (foldl1', nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Kore.Definition.Attributes.Base
import Kore.Definition.Base (KoreDefinition (..))
import Kore.Pattern.Base qualified as Internal
import Kore.Syntax.Json.Base qualified as Syntax

internalisePattern ::
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Pattern
internalisePattern KoreDefinition{sorts, symbols} pat = do
    (terms, constraints) <- internalise' pat

    -- construct an AndTerm from all terms (checking sort consistency)
    term <- andTerm terms
    pure
        Internal.Pattern
        { term
        , constraints
        }


    where
      internaliseSort :: Syntax.Sort -> Except PatternError Internal.Sort
      internaliseSort =
          mapExcept (first PatternSortError) . checkSort mempty sorts

      internalise' ::
          Syntax.KorePattern ->
          Except PatternError ([Internal.Term], [Internal.Predicate])
      internalise' = \case
          Syntax.KJEVar{name, sort} -> do
              variableSort <- internaliseSort sort
              let variableName = fromId name
              term $ Internal.Var Internal.Variable {variableSort, variableName}
          Syntax.KJSVar{name} ->
              throwE $ NotSupported ("Set variable " <> fromId name)

          -- Syntax.KJApp

          Syntax.KJString{value} ->
              term $ Internal.DomainValue (Internal.SortApp "SortString" []) value

          Syntax.KJTop{} -> do
              pre $ Internal.Top

          Syntax.KJBottom{} -> do
              pre $ Internal.Bottom

          Syntax.KJNot{arg} -> do
              (ts, ps) <- internalise' arg
              when (not (null ts) || null ps) $
                  throwE $ PredicateExpected "Not" arg
              pre $ Internal.Not (foldl1' Internal.AndPredicate ps)

          Syntax.KJAnd {sort, first = arg1, second = arg2} -> do
              result1 <- internalise' arg1
              result2 <- internalise' arg2

              -- decide whether to use AndTerm or AndPredicate
              -- no mix of term and predicate allowed here
              case (result1, result2) of
                  (([], []), _) ->
                      throwE $ NoTermFound
                  (_, ([], [])) ->
                      throwE $ NoTermFound
                  ((ts1, []), (ts2, [])) -> do
                      internalSort <- internaliseSort sort
                      term =<< andTerm' internalSort (ts1 <> ts2)
                  (([], ps1), ([], ps2)) ->
                      pres $ ps1 <> ps2
                  ((_:_, _:_), (_:_, [])) -> -- arg1 is both, arg2 is term
                      throwE $ TermExpected "And" arg1
                  ((_:_, []), (_:_, _:_)) -> -- arg2 is both, arg1 is term
                      throwE $ TermExpected "And" arg2
                  ((_:_, _:_), ([], _:_)) -> -- arg1 is both, arg2 is pred
                      throwE $ PredicateExpected "And" arg1
                  (([], _:_), (_:_, _:_)) -> -- arg2 is both, arg1 is pred
                      throwE $ PredicateExpected "And" arg2

          Syntax.KJOr {first = arg1, second = arg2} ->
              pre =<< predicate2 "Or" Internal.Or arg1 arg2

          _ -> throwE $ GeneralError "not implemented"

      term t = pure ([t], [])
      pre p = pure ([], [p])
      pres ps = pure ([], ps)

      andTerm :: [Internal.Term] -> Except PatternError Internal.Term
      andTerm ts = do
          -- check resulting terms for consistency and sorts
          when (null ts) $ throwE NoTermFound

          let sortList = nub $ map sortOfTerm ts

          -- TODO needs to consider sub-sorts instead (set must be
          -- consistent) if this code becomes order-sorted
          unless (length sortList == 1) $
              throwE $ PatternSortError (IncompatibleSorts $ map externaliseSort sortList)
          let andSort = head sortList
          pure $ foldl1' (Internal.AndTerm andSort) ts

      andTerm' ::
          Internal.Sort ->
          [Internal.Term] ->
          Except PatternError Internal.Term
      andTerm' expectedSort ts = do
          result <- andTerm ts
          let resultSort = sortOfTerm result
          unless (expectedSort == resultSort) $
              throwE $ PatternSortError $ IncompatibleSorts $ map externaliseSort [expectedSort, resultSort]
          pure result

      predicate2 :: Text -> (Internal.Predicate -> Internal.Predicate -> Internal.Predicate) -> Syntax.KorePattern -> Syntax.KorePattern -> Except PatternError Internal.Predicate
      predicate2 errMsg con p1 p2 = undefined

----------------------------------------
-- idea for a slightly more robust approach

{- | Decide whether a Syntax pattern is a term or a predicate, or has
   parts of both.
-}
data PatternClass
    = IsTerm Syntax.KorePattern
    | IsPredicate Syntax.KorePattern
    | HasBoth Syntax.KorePattern Syntax.KorePattern
    deriving (Eq, Show)



fromId :: Syntax.Id -> Text
fromId (Syntax.Id n) = n

----------------------------------------
{- | Given a set of sort variable names and a sort attribute map, checks
   a given syntactic @Sort@ and converts to an internal Sort
-}
checkSort ::
    Set Text ->
    Map Internal.SortName SortAttributes ->
    Syntax.Sort ->
    Except SortError Internal.Sort
checkSort knownVars sortMap = check'
  where
    check' :: Syntax.Sort -> Except SortError Internal.Sort
    check' var@Syntax.SortVar{name = Syntax.Id n} = do
        unless (n `Set.member` knownVars) $
            throwE (UnknownSort var)
        pure $ Internal.SortVar n
    check' app@Syntax.SortApp{name = Syntax.Id n, args} =
        do
            maybe
                (throwE $ UnknownSort app)
                ( \SortAttributes{argCount} ->
                    unless (length args == argCount) $
                        throwE (WrongSortArgCount app argCount)
                )
                (Map.lookup n sortMap)
            internalArgs <- mapM check' args
            pure $ Internal.SortApp n internalArgs

externaliseSort :: Internal.Sort -> Syntax.Sort
externaliseSort (Internal.SortApp name args) =
    Syntax.SortApp (Syntax.Id name) (map externaliseSort args)
externaliseSort (Internal.SortVar name) =
    Syntax.SortVar (Syntax.Id name)

----------------------------------------
-- TODO find a better home for this one, maybe Kore.Pattern.Util.
-- We'll need more helpers when we write the actual functionality.
sortOfTerm :: Internal.Term -> Internal.Sort
sortOfTerm (Internal.AndTerm sort _ _) = sort
sortOfTerm (Internal.SymbolApplication sort _ _ _) = sort
sortOfTerm (Internal.DomainValue sort _) = sort
sortOfTerm (Internal.Var Internal.Variable{variableSort}) = variableSort

----------------------------------------
data PatternError
    = NotSupported Text
    | NoTermFound
    | PatternSortError SortError
    | TermExpected Text Syntax.KorePattern
    | PredicateExpected Text Syntax.KorePattern
    | GeneralError Text
    deriving stock (Eq, Show)

data SortError
    = UnknownSort Syntax.Sort
    | WrongSortArgCount Syntax.Sort Int
    | IncompatibleSorts [Syntax.Sort]
    deriving stock (Eq, Show)
