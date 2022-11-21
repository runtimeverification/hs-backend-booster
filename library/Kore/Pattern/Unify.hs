{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Unify (
    module Kore.Pattern.Unify,
) where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base
import Kore.Pattern.Util (freeVariables, sortOfTerm, substituteInTerm)

import Kore.Syntax.Json.Internalise (matchSorts) -- temporary

--
data UnificationResult
    = -- | equal structure (constructors) after substitution (substitution goes both ways)
      UnificationSuccess Substitution
    | -- | different constructors or domain values, or sort mismatch
      UnificationFailed FailReason
    | -- | (other) cases that are unresolved (offending case in head position).
      UnificationRemainder (NonEmpty (Term, Term))
    | InternalError String
    deriving stock (Eq, Show)

-- | Additional information to explain why a unification has failed
data FailReason
    = -- | Unificand sorts differ
      DifferentSorts Term Term
    | -- | (Domain) values differ
      DifferentValues Term Term
    | -- | Symbols differ
      DifferentSymbols Term Term
    | -- | Variable would refer to itself
      VariableRecursion Variable Term
    | -- | Variable reassigned
      VariableConflict Variable Term Term
    deriving stock (Eq, Show)

type Substitution = Map Variable Term

{- | Attempts to find a simple unifying substitution for the given
   terms. Only constructor symbols are considered (no functions).

   The returned substitution is oriented towards 'term1', i.e.,
   prefers to replace its variables if given a choice.
-}
unifyTerms :: KoreDefinition -> Term -> Term -> UnificationResult
unifyTerms KoreDefinition{symbols, sorts} term1 term2 =
    let runUnification :: UnificationState -> UnificationResult
        runUnification =
            fromEither
                . runExcept
                . fmap (UnificationSuccess . uSubstitution)
                . execStateT unification
     in runUnification
            State
                { uSubstitution = Map.empty
                , uTargetVars = freeVariables term1
                , uQueue = Seq.singleton (term1, term2)
                , uSubsorts = Map.map snd sorts
                , uSymbols = Map.map fst symbols
                }

data UnificationState = State
    { uSubstitution :: Substitution
    , uTargetVars :: Set Variable
    , uQueue :: Seq (Term, Term) -- work queue (breadth-first term traversal)
    , uSubsorts :: SortTable
    , uSymbols :: Map SymbolName SymbolAttributes
    }

type SortTable = Map SortName (Set SortName)

unification :: StateT UnificationState (Except UnificationResult) ()
unification = do
    queue <- gets uQueue
    case queue of
        Empty -> pure () -- done
        (term1, term2) :<| _ -> do
            unify1 term1 term2
            unification

unify1 ::
    Term ->
    Term ->
    StateT UnificationState (Except UnificationResult) ()
-- two domain values: have to fully agree
unify1
    d1@(DomainValue s1 t1)
    d2@(DomainValue s2 t2) =
        do
            subsorts <- gets uSubsorts
            unless (sortsAgree subsorts s1 s2) $
                failWith (DifferentSorts d1 d2)
            unless (t1 == t2) $
                failWith (DifferentValues d1 d2)

-- two symbol applications: fail if names differ, recurse
unify1
    t1@(SymbolApplication s1 _argSorts1 symName1 args1)
    t2@(SymbolApplication s2 _argSorts2 symName2 args2) =
        do
            subsorts <- gets uSubsorts
            -- argument sorts have been checked upon internalisation
            unless (sortsAgree subsorts s1 s2) $
                failWith (DifferentSorts t1 t2)
            -- If we have functions, pass - only constructors are matched.
            symbols <- gets uSymbols
            let isConstr sym = maybe False isConstructor $ Map.lookup sym symbols
            unless (isConstr symName1 && isConstr symName2) $
                returnAsRemainder t1 t2
            -- constructors must be the same
            unless (symName1 == symName2) $
                failWith (DifferentSymbols t1 t2)
            zipWithM_ enqueueProblem args1 args2

-- and-term in pattern: must unify with both arguments
unify1
    (AndTerm _ t1a t1b)
    term2 =
        do
            enqueueProblem t1a term2
            enqueueProblem t1b term2
-- and-term in subject: must unify with both arguments
unify1
    term1
    (AndTerm _ t2a t2b) =
        do
            enqueueProblem term1 t2a
            enqueueProblem term1 t2b

-- term1 variable (target): introduce a new binding
unify1
    (Var var@Variable{variableSort})
    term2 =
        do
            subsorts <- gets uSubsorts
            let termSort = sortOfTerm term2
            unless (sortsAgree subsorts variableSort termSort) $
                failWith (DifferentSorts (Var var) term2)
            bindVariable var term2

-- term2 variable (not target), term1 not a variable: add binding
unify1
    term1
    (Var var@Variable{variableSort}) =
        do
            subsorts <- gets uSubsorts
            let termSort = sortOfTerm term1
            unless (sortsAgree subsorts variableSort termSort) $
                failWith (DifferentSorts term1 (Var var))
            bindVariable var term1

-- Remaining other cases: mix of DomainValue and SymbolApplication (either side)
-- The remaining unification problems are returned
unify1
    t1@SymbolApplication{}
    t2@DomainValue{} =
        returnAsRemainder t1 t2
unify1
    t1@DomainValue{}
    t2@SymbolApplication{} =
        returnAsRemainder t1 t2

failWith :: FailReason -> StateT s (Except UnificationResult) ()
failWith = lift . throwE . UnificationFailed

enqueueProblem :: Monad m => Term -> Term -> StateT UnificationState m ()
enqueueProblem term1 term2 =
    modify $ \s@State{uQueue} -> s{uQueue = uQueue :|> (term1, term2)}

{- | Binds a variable to a term to add to the resulting unifier.

 We apply the accumulated substitution whenever a new variable
 binding to a term is added. This avoids repeated traversals while
 guarding against substitution loops.
-}
bindVariable :: Variable -> Term -> StateT UnificationState (Except UnificationResult) ()
bindVariable var term = do
    State{uSubstitution = currentSubst, uTargetVars = targets} <- get
    case term of
        -- Check if term is a variable, prefer target variables. Should
        -- not happen given how we call it in the code above.
        Var var2
            | var2 `Set.member` targets
                && not (var `Set.member` targets) ->
                bindVariable var2 (Var var)
        -- regular case
        _other -> do
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
            modify $ \s -> s{uSubstitution = newSubst}

returnAsRemainder :: Term -> Term -> StateT UnificationState (Except UnificationResult) ()
returnAsRemainder t1 t2 = do
    remainder <- gets uQueue
    lift $ throwE $ UnificationRemainder $ (t1, t2) :| toList remainder

-- FIXME use sort utilities (currently stub code living in Kore.Syntax.Json.Internalise)
sortsAgree :: SortTable -> Sort -> Sort -> Bool
sortsAgree _ s1 s2 = either (const False) (const True) $ runExcept $ matchSorts s1 s2
