{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.UnifiedMatcher (
    MatchResult (..),
    MatchType (..),
    FailReason (..),
    Substitution,
    matchTerms,
    checkSubsort,
    SortError (..),
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Either.Extra
import Data.List.NonEmpty as NE (NonEmpty, fromList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq

import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

import Booster.Definition.Attributes.Base (KListDefinition)
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Util (
    checkSymbolIsAc,
    freeVariables,
    isConstructorSymbol,
    sortOfTerm,
    substituteInTerm,
 )
import Data.ByteString (ByteString)
import Data.List (partition)

-- | Result of matching a pattern to a subject (a substitution or an indication of what went wrong)
data MatchResult
    = -- | equal structure (constructors) after substitution (substitution goes both ways)
      MatchSuccess Substitution
    | -- | different constructors or domain values, or sort mismatch
      MatchFailed FailReason
    | -- | (other) cases that are unresolved (offending case in head position).
      MatchIndeterminate (NonEmpty (Term, Term))
    deriving stock (Eq, Show)

data MatchType = Rule | Fun deriving (Eq)

-- | Additional information to explain why a unification has failed
data FailReason
    = -- | (Domain) values differ
      DifferentValues Term Term
    | -- | Symbols differ
      DifferentSymbols Term Term
    | -- | The unificands have different sorts
      DifferentSorts Term Term
    | -- | Variable would refer to itself. This should not happen
      -- because we rename rule variables to avoid it.
      VariableRecursion Variable Term
    | -- | Variable reassigned
      VariableConflict Variable Term Term
    | -- | Key not found in map
      KeyNotFound Term Term
    | -- | Key not found in map
      DuplicateKeys Term Term
    | -- | Shared variables between matching terms
      SharedVariables (Set Variable)
    | -- | sort error (using variables or unknown sorts)
      SubsortingError SortError
    | -- | The two terms have differing argument lengths
      ArgLengthsDiffer Term Term
    deriving stock (Eq, Show)

instance Pretty FailReason where
    pretty = \case
        DifferentValues t1 t2 ->
            "Values differ:" <> align (sep [pretty t1, pretty t2])
        DifferentSymbols t1 t2 ->
            vsep ["Symbols differ:", pretty t1, pretty t2] -- shorten?
        DifferentSorts t1 t2 ->
            vsep ["Sorts differ:", pretty t1, pretty t2] -- shorten?
        VariableRecursion v t ->
            "Variable recursion found: " <> pretty v <> " in " <> pretty t
        VariableConflict v t1 t2 ->
            vsep
                [ "Variable conflict for " <> pretty v
                , pretty t1
                , pretty t2
                ]
        KeyNotFound k m ->
            vsep
                [ "Key " <> pretty k <> " not found in map"
                , pretty m
                ]
        DuplicateKeys k m ->
            vsep
                [ "Key " <> pretty k <> " appears more than once in map"
                , pretty m
                ]
        SharedVariables vs ->
            "Shared variables:" <+> hsep (map pretty $ Set.toList vs)
        SubsortingError err ->
            pretty $ show err
        ArgLengthsDiffer t1 t2 ->
            vsep ["Argument length differ", pretty t1, pretty t2]

type Substitution = Map Variable Term

{- | Attempts to find a simple unifying substitution for the given
   terms. Only constructor symbols are considered (no functions).

   The returned substitution is oriented towards 'term1', i.e.,
   prefers to replace its variables if given a choice.

   This code calls `error` for internal errors (e.g., function
   arguments that should not be possible), assuming the caller will
   catch and handle those errors.
-}
matchTerms :: MatchType -> KoreDefinition -> Term -> Term -> MatchResult
matchTerms mType KoreDefinition{sorts} term1 term2 =
    let runMatch :: MatchState -> MatchResult
        runMatch =
            fromEither
                . runExcept
                . fmap (MatchSuccess . mSubstitution)
                . execStateT (match mType)
        freeVars1 = freeVariables term1
        freeVars2 = freeVariables term2
        sharedVars = freeVars1 `Set.intersection` freeVars2
     in if not $ Set.null sharedVars
            then
                MatchIndeterminate $
                    NE.fromList
                        [(Var v, Var v) | v <- Set.toList sharedVars]
            else
                runMatch
                    State
                        { mSubstitution = Map.empty
                        , mTargetVars = freeVars1
                        , mQueue = Seq.singleton (term1, term2) -- PriorityQueue.singleton (term1, term2) RegularTerm ()
                        , mMapQueue = mempty
                        , mIndeterminate = []
                        , mSubsorts = Map.map snd sorts
                        }

data MatchState = State
    { mSubstitution :: Substitution
    , mTargetVars :: Set Variable
    , mQueue :: Seq (Term, Term) -- PriorityQueue.HashPSQ (Term, Term) UnificationPriority () -- work queue with a priority
    , mMapQueue :: Seq (Term, Term)
    , mIndeterminate :: [(Term, Term)] -- list of postponed indeterminate terms (function results)
    , mSubsorts :: SortTable
    }

type SortTable = Map SortName (Set SortName)

match :: MatchType -> StateT MatchState (Except MatchResult) ()
match mType = do
    queue <- gets mQueue
    mapQueue <- gets mMapQueue
    case queue of
        Empty -> case mapQueue of
            Empty -> checkIndeterminate -- done
            (term1, term2) :<| rest -> do
                modify $ \s -> s{mMapQueue = rest}
                match1 mType term1 term2
                match mType
        (term1, term2) :<| rest -> do
            -- case PriorityQueue.minView queue of
            --     Nothing -> checkIndeterminate -- done
            --     Just ((term1, term2), _, _, rest) -> do
            modify $ \s -> s{mQueue = rest}
            match1 mType term1 term2
            match mType

checkIndeterminate :: StateT MatchState (Except MatchResult) ()
checkIndeterminate = do
    indeterminate <- gets mIndeterminate
    unless (null indeterminate) . lift $
        throwE (MatchIndeterminate $ NE.fromList indeterminate)
match1 ::
    MatchType ->
    Term ->
    Term ->
    StateT MatchState (Except MatchResult) ()
{- FOURMOLU_DISABLE -}
match1 Rule (AndTerm t1a t1b)                          t2@AndTerm{}                               = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 Fun  t1@AndTerm{}                               t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    (AndTerm t1a t1b)                          t2@DomainValue{}                           = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@Injection{}                             = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@KMap{}                                  = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@KList{}                                 = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@KSet{}                                  = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@ConsApplication{}                       = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _    (AndTerm t1a t1b)                          t2@FunctionApplication{}                   = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 Rule (AndTerm t1a t1b)                          t2@Var{}                                   = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 Fun  t1@AndTerm{}                               t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@DomainValue{}                           (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@DomainValue{}                           t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    (DomainValue s1 t1)                        (DomainValue s2 t2)                        = matchDV s1 t1 s2 t2
match1 _    t1@DomainValue{}                           t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _    t1@DomainValue{}                           t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@DomainValue{}                           t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _    t1@DomainValue{}                           t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@DomainValue{}                           t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _    t1@DomainValue{}                           t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rule t1@DomainValue{}                           (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@DomainValue{}                           t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@Injection{}                             (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@Injection{}                             t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    t1@Injection{}                             t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 mTy  (Injection source1 target1 trm1)           (Injection source2 target2 trm2)           = matchInj mTy source1 target1 trm1 source2 target2 trm2
match1 _    t1@Injection{}                             t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@Injection{}                             t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _    t1@Injection{}                             t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@Injection{}                             t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _    t1@Injection{}                             t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 _    t1@Injection{}                             t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@KMap{}                                  (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@KMap{}                                  t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    t1@KMap{}                                  t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Rule t1@KMap{}                                  t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 Fun  t1@KMap{}                                  t2@Injection{}                             = addIndeterminate t1 t2
match1 _    t1@KMap{}                                  t2@KMap{}                                  = matchMaps t1 t2
match1 _    t1@KMap{}                                  t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _    t1@KMap{}                                  t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@KMap{}                                  t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _    t1@KMap{}                                  t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rule t1@KMap{}                                  (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@KMap{}                                  t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@KList{}                                 (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@KList{}                                 t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    t1@KList{}                                 t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Rule t1@KList{}                                 t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 Fun  t1@KList{}                                 t2@Injection{}                             = addIndeterminate t1 t2
match1 _    t1@KList{}                                 t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 Rule (KList def1 heads1 rest1)                  (KList def2 heads2 rest2)                  = matchLists def1 heads1 rest1 def2 heads2 rest2
match1 Fun  t1@KList{}                                 t2@KList{}                                 = addIndeterminate t1 t2
match1 _    t1@KList{}                                 t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@KList{}                                 t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _    t1@KList{}                                 t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rule t1@KList{}                                 (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@KList{}                                 t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@KSet{}                                  (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@KSet{}                                  t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    t1@KSet{}                                  t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Rule t1@KSet{}                                  t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 Fun  t1@KSet{}                                  t2@Injection{}                             = addIndeterminate t1 t2
match1 _    t1@KSet{}                                  t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@KSet{}                                  t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _    t1@KSet{}                                  t2@KSet{}                                  = addIndeterminate t1 t2
match1 _    t1@KSet{}                                  t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _    t1@KSet{}                                  t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rule t1@KSet{}                                  (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@KSet{}                                  t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@ConsApplication{}                       (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@ConsApplication{}                       t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _    t1@ConsApplication{}                       t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 _    t1@ConsApplication{}                       t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _    t1@ConsApplication{}                       t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _    t1@ConsApplication{}                       t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _    t1@ConsApplication{}                       t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 mTy  (ConsApplication symbol1 sorts1 args1)     (ConsApplication symbol2 sorts2 args2)     = matchSymbolAplications mTy symbol1 sorts1 args1 symbol2 sorts2 args2
match1 Rule t1@ConsApplication{}                       t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Fun  (ConsApplication symbol1 sorts1 args1)     (FunctionApplication symbol2 sorts2 args2) = matchSymbolAplications Fun symbol1 sorts1 args1 symbol2 sorts2 args2
match1 Rule t1@ConsApplication{}                       (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@ConsApplication{}                       t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@FunctionApplication{}                   (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@FunctionApplication{}                   t2@AndTerm{}                               = addIndeterminate t1 t2
match1 Rule t1@FunctionApplication{}                   t2@DomainValue{}                           = addIndeterminate t1 t2
match1 Fun  t1@FunctionApplication{}                   t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Rule t1@FunctionApplication{}                   t2@Injection{}                             = addIndeterminate t1 t2
match1 Fun  t1@FunctionApplication{}                   t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 Rule t1@FunctionApplication{}                   t2@KMap{}                                  = addIndeterminate t1 t2
match1 Fun  t1@FunctionApplication{}                   t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 Rule t1@FunctionApplication{}                   t2@KList{}                                 = addIndeterminate t1 t2
match1 Fun  t1@FunctionApplication{}                   t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 Rule t1@FunctionApplication{}                   t2@KSet{}                                  = addIndeterminate t1 t2
match1 Fun  t1@FunctionApplication{}                   t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 Rule t1@FunctionApplication{}                   t2@ConsApplication{}                       = addIndeterminate t1 t2
match1 Fun  (FunctionApplication symbol1 sorts1 args1) (ConsApplication symbol2 sorts2 args2)     = matchSymbolAplications Fun symbol1 sorts1 args1 symbol2 sorts2 args2
match1 Rule t1@FunctionApplication{}                   t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Fun  (FunctionApplication symbol1 sorts1 args1) (FunctionApplication symbol2 sorts2 args2) = matchSymbolAplications Fun symbol1 sorts1 args1 symbol2 sorts2 args2
match1 Rule t1@FunctionApplication{}                   (Var var2)                                 = matchVar Rule var2 t1
match1 Fun  t1@FunctionApplication{}                   t2@Var{}                                   = addIndeterminate t1 t2
match1 Rule t1@Var{}                                   (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Fun  t1@Var{}                                   t2@AndTerm{}                               = addIndeterminate t1 t2
match1 mTy  (Var var1)                                 t2@DomainValue{}                           = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@Injection{}                             = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@KMap{}                                  = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@KList{}                                 = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@KSet{}                                  = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@ConsApplication{}                       = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@FunctionApplication{}                   = matchVar mTy var1 t2
match1 mTy  (Var var1)                                 t2@Var{}                                   = matchVar mTy var1 t2
{- FOURMOLU_ENABLE -}

matchDV :: Sort -> ByteString -> Sort -> ByteString -> StateT s (Except MatchResult) ()
matchDV s1 t1 s2 t2 =
    do
        unless (t1 == t2) $
            failWith (DifferentValues (DomainValue s1 t1) (DomainValue s2 t2))
        unless (s1 == s2) $ -- sorts must be exactly the same for DVs
            failWith (DifferentSorts (DomainValue s1 t1) (DomainValue s2 t2))
{-# INLINE matchDV #-}

----- Injections
-- two injections. Try to unify the contained terms if the sorts
-- agree. Target sorts must be the same, source sorts may differ if
-- the contained pattern term is just a variable, otherwise they need
-- to be identical.
matchInj ::
    MatchType ->
    Sort ->
    Sort ->
    Term ->
    Sort ->
    Sort ->
    Term ->
    StateT MatchState (Except MatchResult) ()
matchInj
    mType
    source1
    target1
    trm1
    source2
    target2
    trm2
        | target1 /= target2 = do
            failWith (DifferentSorts (Injection source1 target1 trm1) (Injection source2 target2 trm2))
        | source1 == source2 = do
            enqueueRegularProblem trm1 trm2
        | Var v <- trm1 = do
            -- variable in pattern, check source sorts and bind
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchFailed . SubsortingError) $
                    checkSubsort subsorts source2 source1
            if isSubsort
                then bindVariable mType v (Injection source2 source1 trm2)
                else failWith (DifferentSorts trm1 trm2)
        | otherwise =
            failWith (DifferentSorts (Injection source1 target1 trm1) (Injection source2 target2 trm2))
{-# INLINE matchInj #-}

----- Symbol Applications
-- two constructors: fail if names differ, recurse

----- Symbol Applications
matchSymbolAplications ::
    MatchType ->
    Symbol ->
    [Sort] ->
    [Term] ->
    Symbol ->
    [Sort] ->
    [Term] ->
    StateT MatchState (Except MatchResult) ()
matchSymbolAplications
    Rule
    symbol1
    sorts1
    args1
    symbol2
    sorts2
    args2
        | symbol1.name /= symbol2.name =
            failWith
                ( DifferentSymbols (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
                )
        | length args1 /= length args2 =
            failWith $
                ArgLengthsDiffer (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | sorts1 /= sorts2 =
            failWith
                (DifferentSorts (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
        | otherwise =
            enqueueRegularProblems $ Seq.fromList $ zip args1 args2
matchSymbolAplications
    Fun
    symbol1
    sorts1
    args1
    symbol2
    sorts2
    args2
        | symbol1.name /= symbol2.name =
            if isConstructorSymbol symbol1 && isConstructorSymbol symbol2
                then
                    failWith
                        (DifferentSymbols (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
                else addIndeterminate (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | length args1 /= length args2 =
            failWith $
                ArgLengthsDiffer (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | sorts1 /= sorts2 =
            failWith
                (DifferentSorts (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
        -- If the symbol is non-free (AC symbol), return indeterminate
        | checkSymbolIsAc symbol1 =
            addIndeterminate (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | otherwise =
            enqueueRegularProblems $ Seq.fromList $ zip args1 args2

----- Variables

matchVar :: MatchType -> Variable -> Term -> StateT MatchState (Except MatchResult) ()
matchVar
    Rule
    -- twice the exact same variable: verify sorts are equal
    var1@(Variable varSort1 varName1)
    (Var var2@(Variable varSort2 varName2))
        -- same variable: forbidden!
        | var1 == var2 =
            internalError $ "Shared variable: " <> show var1
        | varName1 == varName2 && varSort1 /= varSort2 =
            -- sorts differ, names equal: error!
            failWith $ VariableConflict var1 (Var var1) (Var var2)
matchVar
    -- term1 variable (target): introduce a new binding
    mType
    var@Variable{variableSort}
    term2 =
        do
            let termSort = sortOfTerm term2
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchFailed . SubsortingError) $
                    checkSubsort subsorts termSort variableSort
            if isSubsort
                then bindVariable mType var term2
                else failWith $ DifferentSorts (Var var) term2

-- unification for lists. Only solves simple cases, returns indeterminate otherwise
matchLists ::
    KListDefinition ->
    [Term] ->
    Maybe (Term, [Term]) ->
    KListDefinition ->
    [Term] ->
    Maybe (Term, [Term]) ->
    StateT MatchState (Except MatchResult) ()
matchLists
    def1
    heads1
    rest1
    def2
    heads2
    rest2
        | -- incompatible lists
          def1 /= def2 =
            failWith $ DifferentSorts (KList def1 heads1 rest1) (KList def2 heads2 rest2)
        | -- two fully-concrete lists of the same length
          Nothing <- rest1
        , Nothing <- rest2 =
            if length heads1 == length heads2
                then void $ enqueuePairs heads1 heads2
                else failWith $ DifferentValues (KList def1 heads1 rest1) (KList def2 heads2 rest2)
        | -- left list has a symbolic part, right one is fully concrete
          Just (symb1, tails1) <- rest1
        , Nothing <- rest2 = do
            let emptyList = KList def1 [] Nothing
            remainder <- enqueuePairs heads1 heads2
            case remainder of
                Nothing -- equal head length, rest1 must become .List
                    | null tails1 ->
                        enqueueRegularProblem symb1 emptyList
                    | otherwise -> do
                        -- fully concrete list too short
                        let surplusLeft = KList def1 [] rest1
                        failWith $ DifferentValues surplusLeft emptyList
                Just (Left leftover1) -> do
                    -- fully concrete list too short
                    let surplusLeft = KList def1 leftover1 rest1
                    failWith $ DifferentValues surplusLeft emptyList
                Just (Right leftover2)
                    | null tails1 -> do
                        let newRight = KList def2 leftover2 Nothing
                        enqueueRegularProblem symb1 newRight
                    | otherwise -> do
                        tailRemainder <- -- reversed!
                            enqueuePairs (reverse tails1) (reverse leftover2)
                        case tailRemainder of
                            Nothing ->
                                -- again symb1 needs to become `.List`
                                enqueueRegularProblem symb1 emptyList
                            Just (Left tail1) -> do
                                -- fully concrete list too short
                                let surplusLeft = KList def1 [] $ Just (symb1, reverse tail1)
                                failWith $ DifferentValues surplusLeft emptyList
                            Just (Right tail2) -> do
                                let newRight = KList def2 (reverse tail2) Nothing
                                enqueueRegularProblem symb1 newRight
        | -- mirrored case above: left list fully concrete, right one isn't
          Nothing <- rest1
        , Just _ <- rest2 =
            matchLists def2 heads2 rest2 def1 heads1 rest1 -- won't loop, will fail later if unification succeeds
        | -- two lists with symbolic middle
          Just (symb1, tails1) <- rest1
        , Just (symb2, tails2) <- rest2 = do
            remainder <- enqueuePairs heads1 heads2
            case remainder of
                Nothing -> do
                    -- proceed with tails and then symb
                    tailRem <-
                        fmap (bimap reverse reverse)
                            <$> enqueuePairs (reverse tails1) (reverse tails2)
                    case tailRem of
                        Nothing ->
                            enqueueRegularProblem symb1 symb2
                        Just (Left tails1') -> do
                            let newLeft = KList def1 [] (Just (symb1, tails1'))
                            enqueueRegularProblem newLeft symb2
                        Just (Right tails2') -> do
                            let newRight = KList def2 [] (Just (symb2, tails2'))
                            enqueueRegularProblem symb1 newRight
                Just headRem -> do
                    -- either left or right was longer, remove tails and proceed
                    tailRem <-
                        fmap (bimap reverse reverse)
                            <$> enqueuePairs (reverse tails1) (reverse tails2)
                    case (headRem, tailRem) of
                        (Left heads1', Nothing) -> do
                            let newLeft = KList def1 heads1' (Just (symb1, []))
                            enqueueRegularProblem newLeft symb2
                        (Left heads1', Just (Left tails1')) -> do
                            let newLeft = KList def1 heads1' (Just (symb1, tails1'))
                            enqueueRegularProblem newLeft symb2
                        (Left heads1', Just (Right tails2')) -> do
                            let surplusLeft = KList def1 heads1' (Just (symb1, []))
                                surplusRight = KList def2 [] (Just (symb2, tails2'))
                            addIndeterminate surplusLeft surplusRight
                        (Right heads2', Nothing) -> do
                            let newRight = KList def2 heads2' (Just (symb2, []))
                            enqueueRegularProblem symb1 newRight
                        (Right heads2', Just (Right tails2')) -> do
                            let newRight = KList def2 heads2' (Just (symb2, tails2'))
                            enqueueRegularProblem symb1 newRight
                        (Right heads2', Just (Left tails1')) -> do
                            let surplusLeft = KList def1 [] (Just (symb1, tails1'))
                                surplusRight = KList def2 heads2' (Just (symb2, []))
                            addIndeterminate surplusLeft surplusRight
{-# INLINE matchLists #-}

------ Internalised Maps
matchMaps :: Term -> Term -> StateT MatchState (Except MatchResult) ()
matchMaps
    t1@(KMap def1 _ _)
    t2@(KMap def2 _ _)
        | def1 == def2 = do
            State{mSubstitution = currentSubst, mQueue = queue} <- get
            case queue of
                Empty ->
                    case (substituteInKeys currentSubst t1, substituteInKeys currentSubst t2) of
                        (KMap _ kvs1 rest1, KMap _ kvs2 rest2)
                            | Just duplicate <- duplicateKeys kvs1 -> failWith $ DuplicateKeys duplicate $ KMap def1 kvs1 rest1
                            | Just duplicate <- duplicateKeys kvs2 -> failWith $ DuplicateKeys duplicate $ KMap def1 kvs2 rest2
                            | -- both sets of keys are syntactically the same (some keys could be functions)
                              Set.fromList [k | (k, _v) <- kvs1] == Set.fromList [k | (k, _v) <- kvs2] -> do
                                forM_ (Map.elems $ Map.intersectionWith (,) (Map.fromList kvs1) (Map.fromList kvs2)) $
                                    uncurry enqueueRegularProblem
                                case (rest1, rest2) of
                                    (Just r1, Just r2) -> enqueueRegularProblem r1 r2
                                    (Just r1, Nothing) -> enqueueRegularProblem r1 (KMap def1 [] Nothing)
                                    (Nothing, Just r2) -> enqueueRegularProblem (KMap def1 [] Nothing) r2
                                    (Nothing, Nothing) -> pure ()
                        (KMap _ kvs1 Nothing, KMap _ kvs2 Nothing)
                            | -- the sets of keys do not match but all keys are concrete and fully evaluated
                              -- this means there is a mismatch
                              allKeysConstructorLike kvs1 && allKeysConstructorLike kvs2 ->
                                case kvs1 `findAllKeysIn` kvs2 of
                                    Left notFoundKeys -> failWith $ KeyNotFound (head notFoundKeys) $ KMap def1 kvs2 Nothing
                                    Right (_matched, []) -> error "unreachable case"
                                    Right (_matched, rest) -> failWith $ KeyNotFound (fst $ head rest) $ KMap def1 kvs1 Nothing
                        (KMap _ kvs (Just restVar@Var{}), KMap _ m Nothing)
                            | (cKvs, []) <- partitionConcreteKeys kvs -> unifySimpleMapShape cKvs restVar m
                        (KMap _ m Nothing, KMap _ kvs (Just restVar@Var{}))
                            | (cKvs, []) <- partitionConcreteKeys kvs -> unifySimpleMapShape cKvs restVar m
                        (t1', t2') -> addIndeterminate t1' t2'
                _ ->
                    -- defer unification until all regular terms have unified
                    enqueueMapProblem t1 t2
        | otherwise = failWith $ DifferentSorts t1 t2
      where
        partitionConcreteKeys :: [(Term, Term)] -> ([(Term, Term)], [(Term, Term)])
        partitionConcreteKeys = partition (\(Term attrs _, _) -> attrs.isConstructorLike)

        allKeysConstructorLike :: [(Term, Term)] -> Bool
        allKeysConstructorLike = all (\(Term attrs _, _) -> attrs.isConstructorLike)

        findAllKeysIn :: [(Term, Term)] -> [(Term, Term)] -> Either [Term] ([(Term, Term)], [(Term, Term)])
        findAllKeysIn kvs m =
            let searchMap = Map.fromList kvs
                subjectMap = Map.fromList m
                matchedMap = Map.intersectionWith (,) searchMap subjectMap
                restMap = Map.difference subjectMap matchedMap
                unmatched = Map.keys $ Map.difference searchMap subjectMap
             in if null unmatched
                    then Right (Map.elems matchedMap, Map.toList restMap)
                    else Left unmatched

        duplicateKeys :: [(Term, Term)] -> Maybe Term
        duplicateKeys kvs =
            let duplicates = Map.filter (> (1 :: Int)) $ foldr (flip (Map.insertWith (+)) 1 . fst) mempty kvs
             in case Map.toList duplicates of
                    [] -> Nothing
                    (k, _) : _ -> Just k

        unifySimpleMapShape cKvs restVar m = do
            let (cM, sM) = partitionConcreteKeys m
            case cKvs `findAllKeysIn` cM of
                Left notFoundKeys -> failWith $ KeyNotFound (head notFoundKeys) $ KMap def1 m Nothing
                Right (matched, rest) -> do
                    forM_ matched $ uncurry enqueueRegularProblem
                    enqueueRegularProblem restVar $ KMap def1 (rest ++ sM) Nothing

        substituteInKeys :: Map Variable Term -> Term -> Term
        substituteInKeys substitution = \case
            KMap attrs keyVals rest -> KMap attrs (first (substituteInTerm substitution) <$> keyVals) rest
            other -> other
matchMaps _ _ = undefined
{-# INLINE matchMaps #-}

failWith :: FailReason -> StateT s (Except MatchResult) ()
failWith = lift . throwE . MatchFailed

internalError :: String -> a
internalError = error

enqueueRegularProblem, enqueueMapProblem :: Monad m => Term -> Term -> StateT MatchState m ()
enqueueRegularProblem term1 term2 =
    modify $ \s@State{mQueue} ->
        s
            { mQueue = mQueue :|> (term1, term2)
            }
enqueueMapProblem term1 term2 =
    modify $ \s@State{mMapQueue} ->
        s
            { mMapQueue = mMapQueue :|> (term1, term2)
            }

enqueueRegularProblems :: Monad m => Seq (Term, Term) -> StateT MatchState m ()
enqueueRegularProblems ts =
    modify $ \s@State{mQueue} -> s{mQueue = mQueue >< ts}

{- | pair up the argument lists and enqueue the pairs. If the lists
are of equal length, return Nothing, else return the remaining
terms in the longer list, tagged with their origin).
-}
enqueuePairs ::
    Monad m => [Term] -> [Term] -> StateT MatchState m (Maybe (Either [Term] [Term]))
enqueuePairs ts1 ts2
    | l1 == l2 =
        enqueue ts1 ts2 >> pure Nothing
    | l1 > l2 =
        let (ts1', rest1) = splitAt l2 ts1
         in enqueue ts1' ts2 >> pure (Just $ Left rest1)
    | otherwise -- l1 < l2
        =
        let (ts2', rest2) = splitAt l1 ts2
         in enqueue ts1 ts2' >> pure (Just $ Right rest2)
  where
    l1 = length ts1
    l2 = length ts2
    enqueue xs ys = enqueueRegularProblems $ Seq.fromList $ zip xs ys

-- modify $ \s@State{mQueue} -> s{mQueue = foldr (\(p, t) q -> PriorityQueue.insert t p () q) mQueue ts}

{- | Binds a variable to a term to add to the resulting unifier.

 We apply the accumulated substitution whenever a new variable
 binding to a term is added. This avoids repeated traversals while
 guarding against substitution loops.
-}
bindVariable :: MatchType -> Variable -> Term -> StateT MatchState (Except MatchResult) ()
bindVariable mType var term = do
    State{mSubstitution = currentSubst, mTargetVars = targets} <- get
    case term of
        -- Check if term is a variable, prefer target variables. Should
        -- not happen given how we call it in the code above.
        Var var2
            | mType == Rule
                && var2 `Set.member` targets
                && not (var `Set.member` targets) ->
                bindVariable mType var2 (Var var)
        -- regular case
        _other -> do
            case Map.lookup var currentSubst of
                Just oldTerm
                    | oldTerm == term -> pure () -- already bound
                    | DomainValue{} <- oldTerm
                    , DomainValue{} <- term
                    , mType == Rule ->
                        enqueueRegularProblem oldTerm term
                    | otherwise ->
                        -- the term in the binding could be _equivalent_
                        -- (not necessarily syntactically equal) to term'
                        case mType of
                            Rule -> addIndeterminate oldTerm term
                            Fun -> failWith $ VariableConflict var oldTerm term
                Nothing -> do
                    let
                        -- apply existing substitutions to term
                        term' = substituteInTerm currentSubst term
                    when (var `Set.member` freeVariables term') $
                        failWith (VariableRecursion var term)
                    let
                        -- substitute in existing substitution terms
                        currentSubst' =
                            Map.map (substituteInTerm $ Map.singleton var term') currentSubst
                        newSubst = Map.insert var term' currentSubst'
                    modify $ \s -> s{mSubstitution = newSubst}

addIndeterminate :: Term -> Term -> StateT MatchState (Except MatchResult) ()
addIndeterminate pat subj =
    modify $ \s -> s{mIndeterminate = (pat, subj) : s.mIndeterminate}

{- | Matches a subject sort to a pattern sort, ensuring that the subject
   sort can be used in place of the pattern sort, i.e., is a subsort.

Sort variables are only accepted if they are syntactically identical.
They should not occur in the patterns matched/unified here, and should
not be sent by clients either.
-}
checkSubsort :: SortTable -> Sort -> Sort -> Except SortError Bool
checkSubsort subsorts sub sup
    | sub == sup = pure True
    | SortVar s <- sub = throwE $ FoundSortVariable s
    | SortVar s <- sup = throwE $ FoundSortVariable s
    | SortApp subName subArgs <- sub
    , SortApp supName supArgs <- sup =
        case Map.lookup supName subsorts of
            Nothing ->
                throwE $ FoundUnknownSort sup
            Just result
                | not (subName `Set.member` result) -> pure False
                | otherwise -> do
                    argsCheck <- zipWithM (checkSubsort subsorts) subArgs supArgs
                    pure $ and argsCheck

data SortError
    = FoundSortVariable VarName
    | FoundUnknownSort Sort
    deriving (Eq, Show)
