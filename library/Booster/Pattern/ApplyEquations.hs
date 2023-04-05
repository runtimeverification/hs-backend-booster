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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)

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
applyTermBottomUp
    , applyTermTopDown ::
        forall rule.
        Term ->
        EquationM rule (EquationFailure Term) Term
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
            -- try to apply equations here
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

-- TODO localise?
applyAtTop ::
    forall rule.
    Term ->
    EquationM rule (EquationFailure Term) Term
applyAtTop term = do
    EquationState{definition, theory} <- getState
    let index = termTopIndex term
    when (index == None) $
        throw (IndexIsNone term)
    let idxEquations, anyEquations :: Map Priority [rule]
        idxEquations = fromMaybe Map.empty $ Map.lookup index theory
        anyEquations = fromMaybe Map.empty $ Map.lookup Anything theory
        equationGroups :: [[rule]]
        equationGroups =
            map snd . Map.toAscList $
                if index == Anything
                    then idxEquations
                    else Map.unionWith (<>) idxEquations anyEquations

    -- no need for an error when (null equationGroups), it will just stop.
    processGroup equationGroups
  where
    -- process one group of equations at a time, until something has happened
    processGroup :: [[rule]] -> EquationM rule (EquationFailure Term) Term
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
            (new1 : _more) ->
                -- choose first result if more than one FIXME should
                -- be error for function equations (non-determinism)
                pure new1

applyEquation ::
    Term ->
    RewriteRule tag ->
    EquationM (RewriteRule tag) (EquationFailure Term) (Maybe Term)
applyEquation _term _rule =
    pure Nothing
