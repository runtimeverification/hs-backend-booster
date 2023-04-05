{-# LANGUAGE DeriveFunctor #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.ApplyEquations (
    module Booster.Pattern.ApplyEquations,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Index
import Booster.Pattern.Match

newtype EquationM tag err a = EquationM (StateT (EquationState tag) (Except err) a)
    deriving newtype (Functor, Applicative, Monad)

throw :: err -> EquationM tag err a
throw = EquationM . lift . throwE

data EquationFailure a
    = IndexIsNone a
    | InconsistentFunctionRules [a]
    | IndeterminateResult a a
    | InternalError Text

data EquationState tag = EquationState
    { definition :: KoreDefinition
    , theory :: Theory tag
    , hasChanged :: Bool
    , counter :: Int
    }

startState :: KoreDefinition -> Theory tag -> EquationState tag
startState definition theory =
    EquationState{definition, theory, hasChanged = False, counter = 0}

increment, markChanged, clearChanged :: EquationM tag err ()
increment = EquationM . modify $ \s -> s{counter = 1 + s.counter}
markChanged = EquationM . modify $ \s -> s{hasChanged = True}
clearChanged = EquationM . modify $ \s -> s{hasChanged = False}

getState :: EquationM tag err (EquationState tag)
getState = EquationM get

----------------------------------------

{- | Apply the set of equations in the equation state at all levels of a
   term AST, in the given direction (bottom-up or top-down).

  No iteration happens at the same AST level inside these traversals,
  one equation will be applied per level (if any).
-}
applyTermBottomUp
    , applyTermTopDown ::
        forall tag.
        ApplyEquationOps tag =>
        Term ->
        EquationM (RewriteRule tag) (EquationFailure Term) Term
applyTermBottomUp =
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
            applyAtTop t
applyTermTopDown = \case
    dv@DomainValue{} ->
        pure dv
    v@Var{} ->
        pure v
    Injection src trg t ->
        Injection src trg <$> applyTermTopDown t -- no injection simplification
    AndTerm arg1 arg2 ->
        AndTerm <$> applyTermTopDown arg1 <*> applyTermTopDown arg2 -- no \and simplification
    app@(SymbolApplication sym sorts args) -> do
        -- try to apply equations
        t <- applyAtTop app
        if (getAttributes t).hash /= (getAttributes app).hash
            then do
                markChanged -- or manage this inside applyAtTop?
                case t of
                    SymbolApplication sym' sorts' args' ->
                        SymbolApplication sym' sorts' <$> mapM applyTermTopDown args'
                    _otherwise ->
                        applyTermTopDown t -- won't loop
            else SymbolApplication sym sorts <$> mapM applyTermTopDown args

{- | Try to apply all equations from the state to the given term, in
   priority order and per group.
-}
applyAtTop ::
    forall tag.
    ApplyEquationOps tag =>
    Term ->
    EquationM (RewriteRule tag) (EquationFailure Term) Term
applyAtTop term = do
    EquationState{definition, theory} <- getState
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
    processGroup equationGroups
  where
    -- process one group of equations at a time, until something has happened
    processGroup ::
        [[(RewriteRule tag)]] ->
        EquationM (RewriteRule tag) (EquationFailure Term) Term
    processGroup [] =
        pure term -- nothing to do, term stays the same
    processGroups (eqs : rest) = do
        -- try all equations in this group, and inspect the results
        results <- catMaybes <$> mapM (applyEquation term) eqs
        case results of
            [] ->
                processGroups rest -- no success at all in this group
            [newTerm] ->
                pure newTerm -- single result
            (first : second : more) ->
                onMultipleResults (Proxy @tag) first (second :| more)

applyEquation ::
    forall tag.
    ApplyEquationOps tag =>
    Term ->
    RewriteRule tag ->
    EquationM (RewriteRule tag) (EquationFailure Term) (Maybe Term)
applyEquation term rule = do
    koreDef <- (.definition) <$> getState
    case matchTerm koreDef rule.lhs.term term of
        MatchFailed failReason -> do
            -- some logging, then
            pure Nothing
        MatchIndeterminate pat subj -> do
            -- some logging, then
            onIndeterminateMatch (Proxy @tag) pat subj
        MatchError msg ->
            throw $ InternalError $ "Match error: " <> msg
        MatchSuccess substitution -> do
            -- check conditions, using substitution (will call back
            -- into the simplifier!)
            error "implement me"

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
        EquationM (RewriteRule tag) (EquationFailure Term) Term

    -- | Behaviour when a match cannot be determined
    --
    -- * for '"Simplification"' equations, discard and proceed
    -- * for '"Function"' equations, abort evaluation (equations at
    --   lower priority should not be tried)
    onIndeterminateMatch ::
        Proxy tag ->
        Term ->
        Term ->
        EquationM (RewriteRule tag) (EquationFailure Term) (Maybe Term)

instance ApplyEquationOps "Simplification" where
    -- choose first result if more than one
    onMultipleResults _ one _ = pure one

    -- continue with more equations if application indeterminate
    onIndeterminateMatch _ _ _ = pure Nothing

instance ApplyEquationOps "Function" where
    -- report that equations are non-deterministic
    onMultipleResults _ one (another :| more) =
        -- FIXME should contain the equations not the terms
        throw $ InconsistentFunctionRules (one : another : more)

    -- throw error (abort evaluation) when indeterminate match
    -- (subsequent equations at lower priority cannot be used)
    onIndeterminateMatch _ pat subj =
        -- FIXME should probably mention the equation
        throw $ IndeterminateResult pat subj
