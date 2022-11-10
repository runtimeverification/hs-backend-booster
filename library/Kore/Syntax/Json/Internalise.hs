{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Syntax.Json.Internalise (
    internalisePattern,
    PatternError (..),
    checkSort,
    matchSorts,
    SortError (..),
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Foldable ()

-- foldl1, foldr1
import Data.List (foldl1', nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
internalisePattern KoreDefinition{sorts, symbols} p = do
    (terms, predicates) <- partitionM isTermM $ explodeAnd p

    when (null terms) $ throwE $ NoTermFound p

    -- construct an AndTerm from all terms (checking sort consistency)
    term <- andTerm =<< mapM internaliseTerm terms
    -- internalise all predicates
    constraints <- mapM internalisePredicate predicates
    pure Internal.Pattern{term, constraints}
  where
    internaliseSort :: Syntax.Sort -> Except PatternError Internal.Sort
    internaliseSort =
        mapExcept (first PatternSortError) . checkSort mempty sorts

    -- Throws errors when a predicate is encountered. The 'And' case
    -- should be analysed before, this function produces an 'AndTerm'.
    internaliseTerm ::
        Syntax.KorePattern ->
        Except PatternError Internal.Term
    internaliseTerm pat =
        case pat of
            Syntax.KJEVar{name, sort} -> do
                variableSort <- internaliseSort sort
                let variableName = fromId name
                pure $ Internal.Var Internal.Variable{variableSort, variableName}
            Syntax.KJSVar{} ->
                throwE $ NotSupported pat
            Syntax.KJApp{name, sorts = argSorts, args} -> do
                -- FIXME add sorts to symbol attributes!
                let symbolSort = Internal.SortVar "dunno"
                    symbolArgSorts = []
                SymbolAttributes{} <-
                    maybe (throwE $ UnknownSymbol name) pure $
                        Map.lookup (fromId name) symbols
                internalArgSorts <- mapM internaliseSort argSorts
                -- TODO check that all argument sorts "agree".
                -- Variables can stand for anything but need to be
                -- consistent (a matching problem returning a
                -- substitution)
                sortSubst <-
                    mapExcept (first PatternSortError) $
                        foldM (\subst -> uncurry (matchSorts' subst)) Map.empty $
                            zip symbolArgSorts internalArgSorts
                -- resultSort is the sort read from attributes, with
                -- variables substituted using the arg.sort match
                let resultSort = applySubst sortSubst symbolSort
                Internal.SymbolApplication resultSort
                    <$> mapM internaliseSort argSorts
                    <*> pure (fromId name)
                    <*> mapM internaliseTerm args
            Syntax.KJString{value} ->
                pure $ Internal.DomainValue (Internal.SortApp "SortString" []) value
            Syntax.KJTop{} -> predicate
            Syntax.KJBottom{} -> predicate
            Syntax.KJNot{} -> predicate
            Syntax.KJAnd{sort, first = arg1, second = arg2} -> do
                -- analysed beforehand, expecting this to operate on terms
                a <- internaliseTerm arg1
                b <- internaliseTerm arg2
                resultSort <- internaliseSort sort
                -- TODO check that both a and b are of sort "resultSort"
                -- Which is a unification problem if this involves variables.
                pure $ Internal.AndTerm resultSort a b
            Syntax.KJOr{} -> predicate
            Syntax.KJImplies{} -> predicate
            Syntax.KJIff{} -> predicate
            Syntax.KJForall{} -> predicate
            Syntax.KJExists{} -> predicate
            Syntax.KJMu{} -> predicate
            Syntax.KJNu{} -> predicate
            Syntax.KJCeil{} -> predicate
            Syntax.KJFloor{} -> predicate
            Syntax.KJEquals{} -> predicate
            Syntax.KJIn{} -> predicate
            Syntax.KJNext{} -> predicate
            Syntax.KJRewrites{} -> predicate
            Syntax.KJDV{sort, value} ->
                Internal.DomainValue
                    <$> internaliseSort sort
                    <*> pure value
            Syntax.KJMultiOr{} -> predicate
            Syntax.KJMultiApp{assoc, symbol, sorts = argSorts, argss} ->
                internaliseTerm $ withAssoc assoc (mkF symbol argSorts) argss
      where
        predicate = throwE $ TermExpected pat

    -- Throws errors when a term is encountered. The 'And' case
    -- is analysed before, this function produces an 'AndPredicate'.
    internalisePredicate ::
        Syntax.KorePattern ->
        Except PatternError Internal.Predicate
    internalisePredicate pat = case pat of
        Syntax.KJOr{first = arg1, second = arg2} ->
            predicate2 Internal.Or arg1 arg2
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
        (Internal.Predicate -> Internal.Predicate -> Internal.Predicate) ->
        Syntax.KorePattern ->
        Syntax.KorePattern ->
        Except PatternError Internal.Predicate
    predicate2 con p1 p2 = do
        con <$> internalisePredicate p1 <*> internalisePredicate p2

    -- converts MultiApp and MultiOr to a chain at syntax level
    withAssoc :: Syntax.LeftRight -> (a -> a -> a) -> NonEmpty a -> a
    withAssoc Syntax.Left = foldl1
    withAssoc Syntax.Right = foldr1

    mkF ::
        Syntax.Id ->
        [Syntax.Sort] ->
        Syntax.KorePattern ->
        Syntax.KorePattern ->
        Syntax.KorePattern
    mkF symbol argSorts a b = Syntax.KJApp symbol argSorts [a, b]

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
isTermM pat = case pat of
    Syntax.KJEVar{} -> pure True
    Syntax.KJSVar{} -> throwE $ NotSupported pat
    Syntax.KJApp{} -> pure True
    Syntax.KJString{} -> pure True
    Syntax.KJTop{} -> pure False
    Syntax.KJBottom{} -> pure False
    Syntax.KJNot{} -> pure False
    Syntax.KJAnd{first = arg1, second = arg2} -> do
        a1Term <- isTermM arg1
        a2Term <- isTermM arg2
        when (a1Term /= a2Term) $ throwE (InconsistentPattern pat)
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

{- | Tries to find a substitution of sort variables in the given sort
 pattern (with variables) to match the given subject
-}
matchSorts ::
    -- | Pattern (contains variables to substitute)
    Internal.Sort ->
    -- | Subject (variables here are treated as constants)
    Internal.Sort ->
    Except SortError (Map Internal.VarName Internal.Sort)
matchSorts = matchSorts' Map.empty

{- | Internal matching function starting with a given substitution.
 Tries to find a substitution of sort variables in the given sort
 pattern (with variables) to match the given subject
-}
matchSorts' ::
    -- | starting substitution (accumulated)
    Map Internal.VarName Internal.Sort ->
    -- | Pattern (contains variables to substitute)
    Internal.Sort ->
    -- | Subject (variables here are treated as constants)
    Internal.Sort ->
    Except SortError (Map Internal.VarName Internal.Sort)
matchSorts' subst _pat _subj = do
    pure subst -- FIXME! traverse pattern and subject, consider given starting substitution

{- | Replace occurrences of the map key variable names in the given
 sort by their mapped sorts. There are no local binders so the
 replacement is a straightforward traversal.
-}
applySubst :: Map Internal.VarName Internal.Sort -> Internal.Sort -> Internal.Sort
applySubst subst var@(Internal.SortVar n) =
    fromMaybe var $ Map.lookup n subst
applySubst subst (Internal.SortApp n args) =
    Internal.SortApp n $ map (applySubst subst) args

----------------------------------------
data PatternError
    = NotSupported Syntax.KorePattern
    | NoTermFound Syntax.KorePattern
    | PatternSortError SortError
    | InconsistentPattern Syntax.KorePattern
    | TermExpected Syntax.KorePattern
    | PredicateExpected Syntax.KorePattern
    | UnknownSymbol Syntax.Id
    | GeneralError Text
    deriving stock (Eq, Show)

data SortError
    = UnknownSort Syntax.Sort
    | WrongSortArgCount Syntax.Sort Int
    | IncompatibleSorts [Syntax.Sort]
    deriving stock (Eq, Show)
