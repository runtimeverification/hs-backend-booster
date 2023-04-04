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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)

import Booster.Definition.Base
import Booster.Definition.Attributes.Base
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
    { theory :: Theory tag
    , hasChanged :: Bool
    , counter :: Int
    }

startState :: Theory tag -> EquationState tag
startState theory = EquationState{theory, hasChanged = False, counter = 0}

increment, markChanged, clearChanged :: EquationState tag -> EquationState tag
increment s = s{counter = 1 + s.counter}

markChanged s = s{hasChanged = True}

clearChanged s = s{hasChanged = False}

----------------------------------------
applyTermBottomUp ::
    forall rule.
    (KoreDefinition -> Theory rule) ->
    Term ->
    EquationM rule (EquationFailure Term) Term
applyTermBottomUp selectTheory = undefined -- iterate $ cataA applyAtTopF ???

-- TODO localise?
applyAtTop :: forall rule. Theory rule -> Term -> EquationM rule (EquationFailure Term) Term
applyAtTop theory term = do
    let index = termTopIndex term
    when (index == None) $
        throw $ IndexIsNone term
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
    processGroup [] = pure term -- nothing to do, term stays the same
    processGroups (eqs :: rest) = do
        -- try all equations in this group, and inspect the results
        results <- catMaybes <$> mapM (applyEquation term) eqs


        undefined

applyEquation ::
    Term ->
    RewriteRule tag ->
    EquationM (RewriteRule tag) (EquationFailure Term) (Maybe Term)
applyEquation = undefined
