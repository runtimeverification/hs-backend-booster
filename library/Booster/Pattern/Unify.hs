{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Unify (
    UnificationResult (..),
    FailReason (..),
    Substitution,
    unifyTerms,
    checkSubsort,
    SortError (..),
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.List.NonEmpty as NE (NonEmpty, fromList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq

import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Util (
    freeVariables,
    isFunctionSymbol,
    sortOfTerm,
    substituteInTerm,
 )

-- | Result of a unification (a substitution or an indication of what went wrong)
data UnificationResult
    = -- | equal structure (constructors) after substitution (substitution goes both ways)
      UnificationSuccess Substitution
    | -- | different constructors or domain values, or sort mismatch
      UnificationFailed FailReason
    | -- | (other) cases that are unresolved (offending case in head position).
      UnificationRemainder (NonEmpty (Term, Term))
    | -- | sort error (using variables or unknown sorts)
      UnificationSortError SortError
    deriving stock (Eq, Show)

-- | Additional information to explain why a unification has failed
data FailReason
    = -- | (Domain) values differ
      DifferentValues Term Term
    | -- | Symbols differ
      DifferentSymbols Term Term
    | -- | The unificands have different sorts
      DifferentSorts Term Term
    | -- | Variable would refer to itself. FIXME should not happen at
      -- all. Need to rename rule variables to avoid it.
      VariableRecursion Variable Term
    | -- | Variable reassigned
      VariableConflict Variable Term Term
    | -- | Key not found in map
      KeyNotFound Term Term
    | -- | Key not found in map
      DuplicateKeys Term Term
    deriving stock (Eq, Show)

instance Pretty FailReason where
    pretty (DifferentValues t1 t2) =
        "Values differ:" <> align (sep [pretty t1, pretty t2])
    pretty (DifferentSymbols t1 t2) =
        vsep ["Symbols differ:", pretty t1, pretty t2] -- shorten?
    pretty (DifferentSorts t1 t2) =
        vsep ["Sorts differ:", pretty t1, pretty t2] -- shorten?
    pretty (VariableRecursion v t) =
        "Variable recursion found: " <> pretty v <> " in " <> pretty t
    pretty (VariableConflict v t1 t2) =
        vsep
            [ "Variable conflict for " <> pretty v
            , pretty t1
            , pretty t2
            ]
    pretty (KeyNotFound k m) =
        vsep
            [ "Key " <> pretty k <> " not found in map"
            , pretty m
            ]
    pretty (DuplicateKeys k m) =
        vsep
            [ "Key " <> pretty k <> " appears more than once in map"
            , pretty m
            ]

type Substitution = Map Variable Term

{- | Attempts to find a simple unifying substitution for the given
   terms. Only constructor symbols are considered (no functions).

   The returned substitution is oriented towards 'term1', i.e.,
   prefers to replace its variables if given a choice.

   This code calls `error` for internal errors (e.g., function
   arguments that should not be possible), assuming the caller will
   catch and handle those errors.
-}
unifyTerms :: KoreDefinition -> Term -> Term -> UnificationResult
unifyTerms KoreDefinition{sorts} term1 term2 =
    let runUnification :: UnificationState -> UnificationResult
        runUnification =
            fromEither
                . runExcept
                . fmap (UnificationSuccess . uSubstitution)
                . execStateT unification
        freeVars1 = freeVariables term1
        freeVars2 = freeVariables term2
        sharedVars = freeVars1 `Set.intersection` freeVars2
     in if not $ Set.null sharedVars
            then
                UnificationRemainder $
                    NE.fromList
                        [(Var v, Var v) | v <- Set.toList sharedVars]
            else
                runUnification
                    State
                        { uSubstitution = Map.empty
                        , uTargetVars = freeVars1
                        , uQueue = Seq.singleton (term1, term2) -- PriorityQueue.singleton (term1, term2) RegularTerm ()
                        , uMapQueue = mempty
                        , uIndeterminate = []
                        , uSubsorts = Map.map snd sorts
                        }

data UnificationState = State
    { uSubstitution :: Substitution
    , uTargetVars :: Set Variable
    , uQueue :: Seq (Term, Term) -- PriorityQueue.HashPSQ (Term, Term) UnificationPriority () -- work queue with a priority
    , uMapQueue :: Seq (Term, Term)
    , uIndeterminate :: [(Term, Term)] -- list of postponed indeterminate terms (function results)
    , uSubsorts :: SortTable
    }

type SortTable = Map SortName (Set SortName)

unification :: StateT UnificationState (Except UnificationResult) ()
unification = do
    queue <- gets uQueue
    mapQueue <- gets uMapQueue
    case queue of
        Empty -> case mapQueue of
            Empty -> checkIndeterminate -- done
            (term1, term2) :<| rest -> do
                modify $ \s -> s{uMapQueue = rest}
                unify1 term1 term2
                unification
        (term1, term2) :<| rest -> do
            -- case PriorityQueue.minView queue of
            --     Nothing -> checkIndeterminate -- done
            --     Just ((term1, term2), _, _, rest) -> do
            modify $ \s -> s{uQueue = rest}
            unify1 term1 term2
            unification

checkIndeterminate :: StateT UnificationState (Except UnificationResult) ()
checkIndeterminate = do
    indeterminate <- gets uIndeterminate
    unless (null indeterminate) . lift $
        throwE (UnificationRemainder $ NE.fromList indeterminate)

unify1 ::
    Term ->
    Term ->
    StateT UnificationState (Except UnificationResult) ()
----- Domain values
-- two domain values: have to fully agree
unify1
    d1@(DomainValue s1 t1)
    d2@(DomainValue s2 t2) =
        do
            unless (t1 == t2) $
                failWith (DifferentValues d1 d2)
            unless (s1 == s2) $ -- sorts must be exactly the same for DVs
                failWith (DifferentSorts d1 d2)
----- And Terms
-- and-term in pattern: must unify with both arguments
unify1
    (AndTerm t1a t1b)
    term2 =
        do
            enqueueRegularProblem t1a term2
            enqueueRegularProblem t1b term2
-- and-term in subject: must unify with both arguments
unify1
    term1
    (AndTerm t2a t2b) =
        do
            enqueueRegularProblem term1 t2a
            enqueueRegularProblem term1 t2b
----- Injections
-- two injections. Try to unify the contained terms if the sorts
-- agree. Target sorts must be the same, source sorts may differ if
-- the contained pattern term is just a variable, otherwise they need
-- to be identical.
unify1
    pat@(Injection source1 target1 trm1)
    subj@(Injection source2 target2 trm2)
        | target1 /= target2 = do
            failWith (DifferentSorts pat subj)
        | source1 == source2 = do
            enqueueRegularProblem trm1 trm2
        | Var v <- trm1 = do
            -- variable in pattern, check source sorts and bind
            subsorts <- gets uSubsorts
            isSubsort <-
                lift . withExcept UnificationSortError $
                    checkSubsort subsorts source2 source1
            if isSubsort
                then bindVariable v (Injection source2 source1 trm2)
                else failWith (DifferentSorts trm1 trm2)
        | otherwise =
            failWith (DifferentSorts pat subj)
----- Symbol Applications
-- two symbol applications: fail if names differ, recurse
unify1
    t1@(SymbolApplication symbol1 sorts1 args1)
    t2@(SymbolApplication symbol2 sorts2 args2)
        | isFunctionSymbol symbol1 || isFunctionSymbol symbol2 =
            -- If we have functions, pass, only match constructors.
            -- NB the result is suspended to get more true failures.
            addIndeterminate t1 t2
        | symbol1.name /= symbol2.name = failWith (DifferentSymbols t1 t2)
        | length args1 /= length args2 =
            internalError $
                "Argument counts differ for same constructor" <> show (t1, t2)
        | sorts1 /= sorts2 = failWith (DifferentSorts t1 t2)
        | otherwise =
            enqueueRegularProblems $ Seq.fromList $ zip args1 args2
----- Variables
-- twice the exact same variable: verify sorts are equal
unify1
    (Var var1@(Variable varSort1 varName1 _))
    (Var var2@(Variable varSort2 varName2 _))
        -- same variable: forbidden!
        | var1 == var2 =
            internalError $ "Shared variable: " <> show var1
        | varName1 == varName2 && varSort1 /= varSort2 =
            -- sorts differ, names equal: error!
            failWith $ VariableConflict var1 (Var var1) (Var var2)
-- term1 variable (target): introduce a new binding
unify1
    term1@(Var var@Variable{variableSort})
    term2 =
        do
            let termSort = sortOfTerm term2
            subsorts <- gets uSubsorts
            isSubsort <-
                lift . withExcept UnificationSortError $
                    checkSubsort subsorts termSort variableSort
            if isSubsort
                then bindVariable var term2
                else failWith $ DifferentSorts term1 term2
-- term2 variable (not target), term1 not a variable: swap arguments (won't recurse)
unify1
    term1
    v@Var{} =
        unify1 v term1
-- injection in pattern, no injection in subject: fail (trm cannot be a variable)
unify1
    inj@Injection{}
    trm =
        failWith $ DifferentSymbols inj trm
-- injection in subject but not in pattern: fail (trm cannot be a variable)
unify1
    trm
    inj@Injection{} =
        failWith $ DifferentSymbols trm inj
------ Remaining other cases: mix of DomainValue and SymbolApplication
------ (either side). The remaining unification problems are returned.
unify1
    t1@SymbolApplication{}
    t2@DomainValue{} =
        addIndeterminate t1 t2
unify1
    t1@DomainValue{}
    t2@SymbolApplication{} =
        addIndeterminate t1 t2
unify1
    t1@(KMap def1 _ _)
    t2@(KMap def2 _ _)
        | def1 == def2 = do
            State{uSubstitution = currentSubst, uQueue = queue} <- get
            case queue of
                Empty ->
                    case (substituteInTerm currentSubst t1, substituteInTerm currentSubst t2) of
                        (KMap _ kvs (Just restVar@Var{}), KMap _ m Nothing)
                            | allConstructorLike kvs && allConstructorLike m -> unifySimpleMapShape kvs restVar m
                        (KMap _ m Nothing, KMap _ kvs (Just restVar@Var{}))
                            | allConstructorLike kvs && allConstructorLike m -> unifySimpleMapShape kvs restVar m
                        _ -> addIndeterminate t1 t2
                _ ->
                    -- defer unification until all regular terms have unified
                    enqueueMapProblem t1 t2
        | otherwise = failWith $ DifferentSorts t1 t2
      where
        allConstructorLike :: [(Term, Term)] -> Bool
        allConstructorLike = all (\(Term attrs _, _) -> attrs.isConstructorLike)

        findAllKeys :: [(Term, Term)] -> [(Term, Term)] -> Either [Term] ([(Term, Term)], [(Term, Term)])
        findAllKeys kvs m =
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

        unifySimpleMapShape kvs restVar m
            | Just duplicate <- duplicateKeys kvs =
                failWith $ DuplicateKeys duplicate $ KMap def1 kvs (Just restVar)
            | Just duplicate <- duplicateKeys m = failWith $ DuplicateKeys duplicate $ KMap def1 m Nothing
            | otherwise = case findAllKeys kvs m of
                Left notFoundKeys -> failWith $ KeyNotFound (head notFoundKeys) $ KMap def1 m Nothing
                Right (matched, rest) -> do
                    forM_ matched $ uncurry enqueueRegularProblem
                    enqueueRegularProblem restVar $ KMap def1 rest Nothing
-- could be unifying a map with a function which returns a map
unify1
    t1@SymbolApplication{}
    t2@KMap{} =
        addIndeterminate t1 t2
unify1
    t1@KMap{}
    t2@SymbolApplication{} =
        addIndeterminate t1 t2
unify1
    trm
    m@KMap{} =
        failWith $ DifferentSymbols trm m
unify1
    m@KMap{}
    trm =
        failWith $ DifferentSymbols m trm

failWith :: FailReason -> StateT s (Except UnificationResult) ()
failWith = lift . throwE . UnificationFailed

internalError :: String -> a
internalError = error

enqueueRegularProblem, enqueueMapProblem :: Monad m => Term -> Term -> StateT UnificationState m ()
enqueueRegularProblem term1 term2 =
    modify $ \s@State{uQueue} ->
        s
            { uQueue = uQueue :|> (term1, term2)
            }
enqueueMapProblem term1 term2 =
    modify $ \s@State{uMapQueue} ->
        s
            { uMapQueue = uMapQueue :|> (term1, term2)
            }

enqueueRegularProblems :: Monad m => Seq (Term, Term) -> StateT UnificationState m ()
enqueueRegularProblems ts =
    modify $ \s@State{uQueue} -> s{uQueue = uQueue >< ts}

-- modify $ \s@State{uQueue} -> s{uQueue = foldr (\(p, t) q -> PriorityQueue.insert t p () q) uQueue ts}

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
            case Map.lookup var currentSubst of
                Just oldTerm
                    | oldTerm == term -> pure () -- already bound
                    | otherwise ->
                        -- the term in the binding could be _equivalent_
                        -- (not necessarily syntactically equal) to term'
                        failWith $ VariableConflict var oldTerm term
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
                    modify $ \s -> s{uSubstitution = newSubst}

addIndeterminate :: Term -> Term -> StateT UnificationState (Except UnificationResult) ()
addIndeterminate pat subj =
    modify $ \s -> s{uIndeterminate = (pat, subj) : s.uIndeterminate}

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
