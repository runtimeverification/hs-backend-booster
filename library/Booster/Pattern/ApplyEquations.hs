{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.ApplyEquations (
    simplify,
    evaluateTerm,
    Direction (..),
    EquationPreference (..),
    EquationFailure (..),
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Functor.Foldable
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.LLVM
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base
import Booster.Pattern.Index
import Booster.Pattern.Match
import Booster.Pattern.Simplify
import Booster.Pattern.Util

newtype EquationM a = EquationM (StateT EquationState (Except EquationFailure) a)
    deriving newtype (Functor, Applicative, Monad)

throw :: EquationFailure -> EquationM a
throw = EquationM . lift . throwE

data EquationFailure
    = IndexIsNone Term
    | InconsistentFunctionRules [Term]
    | IndeterminateMatch Term Term
    | IndeterminateCondition Predicate
    | TooManyIterations Int Term Term
    | EquationLoop [Term]
    | InternalError Text
    deriving stock (Eq, Show)

data EquationState = EquationState
    { definition :: KoreDefinition
    , llvmApi :: Maybe LLVM.API
    , termStack :: [Term]
    }

startState :: KoreDefinition -> Maybe LLVM.API -> EquationState
startState definition llvmApi =
    EquationState{definition, llvmApi, termStack = []}

getState :: EquationM EquationState
getState = EquationM get

countSteps :: EquationM Int
countSteps = length . (.termStack) <$> getState

pushTerm :: Term -> EquationM ()
pushTerm t = EquationM . modify $ \s -> s{termStack = t : s.termStack}

checkForLoop :: Term -> EquationM ()
checkForLoop t = do
    stack <- (.termStack) <$> getState
    whenJust (elemIndex t stack) $ \i ->
        throw (EquationLoop . reverse $ t : take (i + 1) stack)

data Direction = TopDown | BottomUp
    deriving stock (Eq, Show)

data EquationPreference = PreferFunctions | PreferSimplifications
    deriving stock (Eq, Show)

runEquationM ::
    KoreDefinition ->
    Maybe LLVM.API ->
    EquationM a ->
    Either EquationFailure a
runEquationM definition llvmApi (EquationM m) =
    runExcept $ evalStateT m $ startState definition llvmApi

iterateEquations ::
    Int ->
    Direction ->
    EquationPreference ->
    Term ->
    EquationM Term
iterateEquations maxIterations direction preference startTerm =
    go startTerm
  where
    go :: Term -> EquationM Term
    go currentTerm
        | (getAttributes currentTerm).isEvaluated = pure currentTerm
        | otherwise = do
            currentCount <- countSteps
            when (currentCount > maxIterations) $
                throw $
                    TooManyIterations currentCount startTerm currentTerm
            pushTerm currentTerm
            -- evaluate functions and simplify (order parameterised)
            (newTerm, changeFlag) <-
                case preference of
                    PreferFunctions -> do
                        result@(_, changeFlag) <- runWith (.functionEquations) currentTerm
                        if changeFlag
                            then pure result
                            else runWith (.simplifications) currentTerm
                    PreferSimplifications -> do
                        result@(_, changeFlag) <- runWith (.simplifications) currentTerm
                        if changeFlag
                            then pure result
                            else runWith (.functionEquations) currentTerm
            if changeFlag
                then checkForLoop newTerm >> go newTerm
                else pure currentTerm

    runWith ::
        ApplyEquationOps tag =>
        (KoreDefinition -> Theory (RewriteRule tag)) ->
        Term ->
        EquationM (Term, Bool)
    runWith getTheory t = do
        theory <- getTheory . (.definition) <$> getState
        new <- applyTerm theory direction t
        pure (new, new /= t)

----------------------------------------
-- Interface functions
evaluateTerm
    , simplify ::
        Direction ->
        KoreDefinition ->
        Maybe LLVM.API ->
        Term ->
        Either EquationFailure Term
simplify direction def llvmApi =
    runEquationM def{functionEquations = Map.empty} llvmApi
        . iterateEquations 20 direction PreferSimplifications
evaluateTerm direction def llvmApi =
    runEquationM def llvmApi
        . iterateEquations 100 direction PreferFunctions

----------------------------------------

{- | Apply the set of equations in the equation state at all levels of a
   term AST, in the given direction (bottom-up or top-down).

  No iteration happens at the same AST level inside these traversals,
  one equation will be applied per level (if any).
-}
applyTerm ::
    forall tag.
    ApplyEquationOps tag =>
    Theory (RewriteRule tag) ->
    Direction ->
    Term ->
    EquationM Term
applyTerm theory BottomUp =
    cataA $ \case
        DomainValueF s val ->
            pure $ DomainValue s val
        VarF var ->
            pure $ Var var
        InjectionF src trg t ->
            Injection src trg <$> t -- no injection simplification
        AndTermF arg1 arg2 ->
            AndTerm <$> arg1 <*> arg2 -- no \and simplification
        SymbolApplicationF sym sorts args -> do
            t <- SymbolApplication sym sorts <$> sequence args
            applyAtTop theory t
applyTerm theory TopDown = \t@(Term attributes _) ->
    if attributes.isEvaluated
        then pure t
        else do
            s <- getState
            -- All fully concrete values go to the LLVM backend (top-down only)
            if isConcrete t && isJust s.llvmApi
                then pure $ simplifyTerm (fromJust s.llvmApi) s.definition t (sortOfTerm t)
                else applyEquations t
  where
    applyEquations = \case
        dv@DomainValue{} ->
            pure dv
        v@Var{} ->
            pure v
        Injection src trg t ->
            Injection src trg <$> applyTerm theory TopDown t -- no injection simplification
        AndTerm arg1 arg2 ->
            AndTerm -- no \and simplification
                <$> applyTerm theory TopDown arg1
                <*> applyTerm theory TopDown arg2
        app@(SymbolApplication sym sorts args) -> do
            -- try to apply equations
            t <- applyAtTop theory app
            if t /= app
                then do
                    case t of
                        SymbolApplication sym' sorts' args' ->
                            SymbolApplication sym' sorts'
                                <$> mapM (applyTerm theory TopDown) args'
                        _otherwise ->
                            applyTerm theory TopDown t -- won't loop
                else
                    SymbolApplication sym sorts
                        <$> mapM (applyTerm theory TopDown) args

{- | Try to apply all equations from the state to the given term, in
   priority order and per group.
-}
applyAtTop ::
    forall tag.
    ApplyEquationOps tag =>
    Theory (RewriteRule tag) ->
    Term ->
    EquationM Term
applyAtTop theory term = do
    let index = termTopIndex term
    when (index == None) $
        throw (IndexIsNone term)
    let idxEquations, anyEquations :: Map Priority [RewriteRule tag]
        idxEquations = fromMaybe Map.empty $ Map.lookup index theory
        anyEquations = fromMaybe Map.empty $ Map.lookup Anything theory
        equationGroups :: [[RewriteRule tag]]
        equationGroups =
            map snd . Map.toAscList $
                if index == Anything
                    then idxEquations
                    else Map.unionWith (<>) idxEquations anyEquations

    -- no need for an error when (null equationGroups), it will just stop.
    processGroups equationGroups
  where
    -- process one group of equations at a time, until something has happened
    processGroups ::
        [[RewriteRule tag]] ->
        EquationM Term
    processGroups [] =
        pure term -- nothing to do, term stays the same
    processGroups (eqs : rest) = do
        -- try all equations in this group, and inspect the results
        results <- catMaybes <$> mapM (applyEquation term) eqs
        case results of
            [] ->
                processGroups rest -- no success at all in this group
            [newTerm] -> do
                pure newTerm -- single result
            (first : second : more) -> do
                onMultipleResults (Proxy @tag) first (second :| more)

applyEquation ::
    forall tag.
    ApplyEquationOps tag =>
    Term ->
    RewriteRule tag ->
    EquationM (Maybe Term)
applyEquation term rule = runMaybeT $ do
    -- ensured by internalisation: no existentials in equations
    unless (null rule.existentials) $
        lift . throw . InternalError $
            "Equation with existentials: " <> Text.pack (show rule)
    -- immediately cancel if not preserving definedness
    guard rule.computedAttributes.preservesDefinedness
    -- match lhs
    koreDef <- (.definition) <$> lift getState
    case matchTerm koreDef rule.lhs.term term of
        MatchFailed _failReason -> do
            -- some logging, then
            fail "match failed"
        MatchIndeterminate pat subj -> do
            -- some logging, then
            onIndeterminateMatch (Proxy @tag) pat subj
        MatchError msg ->
            lift . throw . InternalError $ "Match error: " <> msg
        MatchSuccess subst -> do
            -- check conditions, using substitution (will call back
            -- into the simplifier! -> import loop)
            let newConstraints =
                    concatMap (splitBoolPredicates . substituteInPredicate subst) $
                        rule.lhs.constraints <> rule.rhs.constraints
            unclearConditions <- catMaybes <$> mapM checkConstraint newConstraints

            unless (null unclearConditions) $
                onIndeterminateCondition (Proxy @tag) (head unclearConditions)
            let rewritten =
                    substituteInTerm subst rule.rhs.term
            -- NB no new constraints, as they have been checked to be `Top`
            -- FIXME what about symbolic constraints here?
            return rewritten
  where
    -- evaluate/simplify a predicate, cut the operation short when it
    -- is Bottom.
    checkConstraint ::
        Predicate ->
        MaybeT EquationM (Maybe Predicate)
    checkConstraint p = do
        mApi <- (.llvmApi) <$> lift getState
        case simplifyPredicate mApi p of
            Bottom -> fail "side condition was false"
            Top -> pure Nothing
            _other -> pure $ Just p

--------------------------------------------------------------------

-- | Helper pattern for simplifyConstraint until predicates have a simpler representation
pattern TrueBool :: Term
pattern TrueBool = DomainValue SortBool "true"

pattern FalseBool :: Term
pattern FalseBool = DomainValue SortBool "false"

{- | Simplification for boolean predicates

  This is used inside function evaluation as well as simplification,
  so it should run using the same state as the caller instead of
  running nested but needs to both evaluate and simplify.

  Outer MaybeT: failure indicates a constraint was false
  Inner Maybe: Nothing if constraint was true, otherwise simplified constraint
-}
_simplifyConstraint ::
    Predicate ->
    EquationM Predicate
--  Awaiting a simplier representation of constraints, we are assuming
--  all predicates are of the form 'P ==Bool true' and evaluating them
--  using simplifyBool if they are concrete.
_simplifyConstraint = \case
    EqualsTerm t TrueBool
        | isConcrete t -> do
            mbApi <- (.llvmApi) <$> getState
            case mbApi of
                Just api ->
                    if simplifyBool api t
                        then pure Top
                        else pure Bottom
                Nothing ->
                    evalBool t >>= prune
        | otherwise ->
            evalBool t >>= prune
    EqualsTerm TrueBool t ->
        -- although "true" is usually 2nd
        _simplifyConstraint (EqualsTerm t TrueBool)
    other ->
        pure other -- should not occur, predicates should be '_ ==Bool true'
  where
    prune =
        pure . \case
            TrueBool -> Top
            FalseBool -> Bottom
            other -> EqualsTerm other TrueBool
    evalBool :: Term -> EquationM Term
    evalBool t = do
        prior <- getState -- save state before so we can "switch"
        -- between evaluate and simplify modes
        result <- pure t -- FIXME simplify and evaluate here
        EquationM $ put prior
        pure result

--------------------------------------------------------------------

{- | Type class to encapsulate the differences between applying
   simplifications and applying function rules.
-}
class ApplyEquationOps (tag :: k) where
    -- | Behaviour when several equations in a priority group match:
    --
    -- * for '"Simplification"' equations, choose the first matching
    --   equation
    -- * for '"Function"' equations, having several equations at the
    --   same priority match is an error, and equations are reported.
    onMultipleResults ::
        Proxy tag ->
        Term ->
        NonEmpty Term ->
        EquationM Term

    -- | Behaviour when a match cannot be determined
    --
    -- * for '"Simplification"' equations, discard and proceed
    -- * for '"Function"' equations, abort evaluation (equations at
    --   lower priority should not be tried)
    onIndeterminateMatch ::
        Proxy tag ->
        Term ->
        Term ->
        MaybeT EquationM Term

    -- | Behaviour when side conditions cannot be determined
    --
    -- * for '"Simplification"' equations, discard and proceed
    -- * for '"Function"' equations, abort evaluation (equations at
    --   lower priority should not be tried)
    onIndeterminateCondition ::
        Proxy tag ->
        Predicate ->
        MaybeT EquationM ()

instance ApplyEquationOps "Simplification" where
    -- choose first result if more than one
    onMultipleResults _ one _ = pure one

    -- continue with more equations if application indeterminate
    onIndeterminateMatch _ _ _ = fail "indeterminate match"
    onIndeterminateCondition _ _ = fail "indeterminate condition"

instance ApplyEquationOps "Function" where
    -- report that equations are non-deterministic
    onMultipleResults _ one (another :| more) =
        -- FIXME should contain the equations not the terms
        throw $ InconsistentFunctionRules (one : another : more)

    -- throw error (abort evaluation) when indeterminate match
    -- (subsequent equations at lower priority cannot be used)
    onIndeterminateMatch _ pat subj =
        lift $ throw $ IndeterminateMatch pat subj

    -- abort further evaluation when a side condition is indeterminate
    -- (subsequent equations at lower priority cannot be used)
    onIndeterminateCondition _ p =
        lift $ throw $ IndeterminateCondition p
