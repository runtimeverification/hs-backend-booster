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
import Control.Monad.Extra
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
    (terms, predicates) <- partitionM isTermM $ explodeAnd pat

    when (null terms) $ throwE $ NoTermFound pat

    -- construct an AndTerm from all terms (checking sort consistency)
    term <- andTerm =<< mapM internaliseTerm terms
    -- internalise all predicates
    constraints <- mapM internalisePredicate predicates
    pure Internal.Pattern{term, constraints}
  where
    internaliseSort :: Syntax.Sort -> Except PatternError Internal.Sort
    internaliseSort =
        mapExcept (first PatternSortError) . checkSort mempty sorts

    internaliseTerm ::
        Syntax.KorePattern ->
        Except PatternError Internal.Term
    internaliseTerm = \case
        Syntax.KJEVar{name, sort} -> do
            variableSort <- internaliseSort sort
            let variableName = fromId name
            pure $ Internal.Var Internal.Variable{variableSort, variableName}
        Syntax.KJString{value} ->
            pure $ Internal.DomainValue (Internal.SortApp "SortString" []) value
    -- ...

    internalisePredicate ::
        Syntax.KorePattern ->
        Except PatternError Internal.Predicate
    internalisePredicate = \case
        Syntax.KJOr{first = arg1, second = arg2} ->
            predicate2 "Or" Internal.Or arg1 arg2
        Syntax.KJTop{} -> do
            pure Internal.Top
        Syntax.KJBottom{} -> do
            pure Internal.Bottom
        Syntax.KJNot{arg} -> do
            Internal.Not <$> internalisePredicate arg
    -- ...

    andTerm :: [Internal.Term] -> Except PatternError Internal.Term
    andTerm [] = error "BUG: andTerm called with empty term list"
    andTerm ts = do
        -- check resulting terms for consistency and sorts
        let sortList = nub $ map sortOfTerm ts

        -- TODO needs to consider sub-sorts instead (set must be
        -- consistent) if this code becomes order-sorted
        unless (length sortList == 1) $
            throwE $
                PatternSortError (IncompatibleSorts $ map externaliseSort sortList)
        let andSort = head sortList
        pure $ foldl1' (Internal.AndTerm andSort) ts

    predicate2 ::
        Text ->
        (Internal.Predicate -> Internal.Predicate -> Internal.Predicate) ->
        Syntax.KorePattern ->
        Syntax.KorePattern ->
        Except PatternError Internal.Predicate
    predicate2 errMsg con p1 p2 = do
        throwE $ GeneralError "implement me!"

----------------------------------------

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

isTermM :: Syntax.KorePattern -> Except PatternError Bool
isTermM = \case
    Syntax.KJEVar{} -> pure True
    svar@Syntax.KJSVar{name} -> throwE $ NotSupported svar
    Syntax.KJApp{} -> pure True
    Syntax.KJString{} -> pure True
    Syntax.KJTop{} -> pure False
    Syntax.KJBottom{} -> pure False
    Syntax.KJNot{} -> pure False
    and@Syntax.KJAnd{first = arg1, second = arg2} -> do
        a1Term <- isTermM arg1
        a2Term <- isTermM arg2
        when (a1Term /= a2Term) $ throwE (InconsistentPattern and)
        pure a1Term
    Syntax.KJOr{} -> pure False
    Syntax.KJImplies{} -> pure False
    Syntax.KJIff{} -> pure False
    Syntax.KJForall{} -> pure False
    Syntax.KJExists{} -> pure False
    Syntax.KJMu{} -> pure False
    Syntax.KJNu{} -> pure False
    Syntax.KJCeil{} -> pure False
    Syntax.KJFloor{} -> pure False
    Syntax.KJEquals{} -> pure False
    Syntax.KJIn{} -> pure False
    Syntax.KJNext{} -> pure False
    Syntax.KJRewrites{} -> pure False
    Syntax.KJDV{} -> pure True
    Syntax.KJMultiOr{} -> pure False
    Syntax.KJMultiApp{} -> pure True

{- | unpacks the top level of 'And' nodes of a 'KorePattern', returning
   the arguments as a list. The top-level sorts of the 'And' nodes are
   ignored, no checks are performed.
-}
explodeAnd :: Syntax.KorePattern -> [Syntax.KorePattern]
explodeAnd Syntax.KJAnd{first = arg1, second = arg2} =
    explodeAnd arg1 <> explodeAnd arg2
explodeAnd other = [other]

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
    = NotSupported Syntax.KorePattern
    | NoTermFound Syntax.KorePattern
    | PatternSortError SortError
    | InconsistentPattern Syntax.KorePattern
    | TermExpected Text Syntax.KorePattern
    | PredicateExpected Text Syntax.KorePattern
    | GeneralError Text
    deriving stock (Eq, Show)

data SortError
    = UnknownSort Syntax.Sort
    | WrongSortArgCount Syntax.Sort Int
    | IncompatibleSorts [Syntax.Sort]
    deriving stock (Eq, Show)
