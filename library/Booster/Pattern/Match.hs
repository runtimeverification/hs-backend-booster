{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Match (
    module Booster.Pattern.Match,
) where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.List.NonEmpty as NE (fromList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Unify (FailReason (..), checkSubsort)
import Booster.Pattern.Util (
    freeVariables,
    isFunctionSymbol,
    sortOfTerm,
    substituteInTerm,
 )

-- | Result of matching a pattern to a subject (unification, failure, or indeterminate)
data MatchResult
    = -- | found a matching substitution
      MatchSuccess (Map Variable Term)
    | -- | pattern and subject have differences (using same failure type as unification)
      MatchFailed FailReason
    | -- | match cannot be determined (not used at the moment)
      MatchIndeterminate Term Term
    | -- | internal errors (invariants violated etc)
      MatchError Text

{- | Attempts to find a matching substitution for the given
   term1 to term2.

  Symbols (functions and constructors) are matched syntactically,
  i.e., when present in the pattern (term1) they also need to be
  present in the subject (term2).

-}
matchTerm :: KoreDefinition -> Term -> Term -> MatchResult
matchTerm KoreDefinition{sorts} term1 term2 =
    let runUnification :: MatchState -> MatchResult
        runUnification =
            fromEither
                . runExcept
                . fmap (MatchSuccess . mSubstitution)
                . execStateT matching
        freeVars1 = freeVariables term1
        freeVars2 = freeVariables term2
        sharedVars = freeVars1 `Set.intersection` freeVars2
     in if not $ Set.null sharedVars
            then
                MatchError . Text.pack . ("Shared Variables: " <>) . show $
                    NE.fromList
                        [(Var v, Var v) | v <- Set.toList sharedVars]
            else
                runUnification
                    State
                        { mSubstitution = Map.empty
                        , mQueue = Seq.singleton (term1, term2)
                        , mSubsorts = Map.map snd sorts
                        }


data MatchState = State
    { mSubstitution :: Map Variable Term
    , mQueue :: Seq (Term, Term) -- work queue
    , mSubsorts :: Map SortName (Set SortName)
    }

matching :: StateT MatchState (Except MatchResult) ()
matching = do
    queue <- gets mQueue
    case queue of
        Empty -> pure () -- done
        (term1, term2) :<| rest -> do
            modify $ \s -> s{mQueue = rest}
            match1 term1 term2
            matching

match1 ::
    Term ->
    Term ->
    StateT MatchState (Except MatchResult) ()
----- Variables
-- term1 variable (target): check sorts, introduce a new binding
match1
    term1@(Var var@Variable{variableSort})
    term2 =
        do
            let termSort = sortOfTerm term2
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchError . Text.pack . show) $
                    checkSubsort subsorts termSort variableSort
            unless isSubsort $
                failWith $ DifferentSorts term1 term2
            -- TODO are subsorts allowed?
            bindVariable
                var
                (if termSort == variableSort
                    then term2
                    else Injection termSort variableSort term2
                )
----- Domain values
-- two domain values: have to fully agree
match1
    d1@(DomainValue s1 t1)
    d2@(DomainValue s2 t2) =
        do
            unless (t1 == t2) $
                failWith (DifferentValues d1 d2)
            unless (s1 == s2) $ -- sorts must be exactly the same for DVs
                failWith (DifferentSorts d1 d2)
-- subject not a domain value
match1
    d1@DomainValue{}
    term2 =
        failWith $ DifferentValues d1 term2
----- And Terms
-- and-term in pattern: must unify with both arguments (typically used
-- to bind variables while also matching)
match1
    (AndTerm t1a t1b)
    term2 =
        do
            enqueueProblem t1a term2
            enqueueProblem t1b term2
----- Injections
-- two injections. Try to unify the contained terms if the sorts
-- agree. Target sorts must be the same, source sorts may differ if
-- the contained pattern term is just a variable, otherwise they need
-- to be identical.
match1
    pat@(Injection source1 target1 trm1)
    subj@(Injection source2 target2 trm2)
        | target1 /= target2 = do
            failWith (DifferentSorts pat subj)
        | source1 == source2 = do
            enqueueProblem trm1 trm2
        | Var v <- trm1 = do
            -- variable in pattern, check source sorts and bind
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchError . Text.pack . show) $
                    checkSubsort subsorts source2 source1
            if isSubsort
                then bindVariable v (Injection source2 source1 trm2)
                else failWith (DifferentSorts trm1 trm2)
        | otherwise =
            failWith (DifferentSorts pat subj)
-- injection in pattern, no injection in subject: fail
match1
    inj@Injection{}
    trm =
        failWith $ DifferentSymbols inj trm
----- Symbol Applications
-- two symbol applications: fail if names differ, recurse
match1
    t1@(SymbolApplication symbol1 sorts1 args1)
    t2@(SymbolApplication symbol2 sorts2 args2)
        | symbol1.name /= symbol2.name = failWith (DifferentSymbols t1 t2)
        | length args1 /= length args2 =
            lift . throwE . MatchError . Text.pack $
                "Argument counts differ for same symbol" <> show (t1, t2)
        | sorts1 /= sorts2 = failWith (DifferentSorts t1 t2)
        | isFunctionSymbol symbol1 =
            -- If the symbol is non-free (AC symbol, return indeterminate)
            lift $ throwE $ MatchIndeterminate t1 t2
        | otherwise =
            enqueueProblems $ Seq.fromList $ zip args1 args2
-- subject not a symbol application: fail
match1
    t1@SymbolApplication{}
    t2 =
        failWith $ DifferentSymbols t1 t2

failWith :: FailReason -> StateT s (Except MatchResult) ()
failWith = lift . throwE . MatchFailed

enqueueProblem :: Monad m => Term -> Term -> StateT MatchState m ()
enqueueProblem term1 term2 =
    modify $ \s@State{mQueue} -> s{mQueue = mQueue :|> (term1, term2)}

enqueueProblems :: Monad m => Seq (Term, Term) -> StateT MatchState m ()
enqueueProblems ts =
    modify $ \s@State{mQueue} -> s{mQueue = mQueue >< ts}

{- | Binds a variable to a term to add to the resulting unifier.

 We apply the accumulated substitution whenever a new variable
 binding to a term is added. This avoids repeated traversals while
 guarding against substitution loops.
-}
bindVariable :: Variable -> Term -> StateT MatchState (Except MatchResult) ()
bindVariable var term = do
    currentSubst <- gets mSubstitution
    let mbOldTerm = Map.lookup var currentSubst
    whenJust mbOldTerm $ \oldTerm ->
        -- TODO the term in the binding could be _equivalent_
        -- (not necessarily syntactically equal) to term'
        failWith $ VariableConflict var oldTerm term
    let -- apply existing substitutions to term
        term' = substituteInTerm currentSubst term
    when (var `Set.member` freeVariables term') $
        failWith (VariableRecursion var term)
    let -- substitute in existing substitution terms
        currentSubst' = Map.map (substituteInTerm $ Map.singleton var term') currentSubst
    let newSubst = Map.insert var term' currentSubst'
    modify $ \s -> s{mSubstitution = newSubst}
