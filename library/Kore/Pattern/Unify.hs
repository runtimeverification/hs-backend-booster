{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Unify (
    module Kore.Pattern.Unify,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.Maybe (listToMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)

import Kore.Pattern.Base

--
data UnificationResult
    = UnificationSuccess Substitution -- same constructors (substitution goes both ways)
    | UnificationFailed -- different constructors
    | UnificationRemainder (Set (Term, Term)) -- (other) cases that are not resolved
    deriving stock (Eq, Show)

type Substitution = Map VarName Term

{- | Attempts to find a simple unifying substitution for the given
   terms. Only constructor symbols are considered (no functions).

   The returned substitution is oriented towards 'term1', i.e.,
   prefers to replace its variables if given a choice.
-}
unifyTerms :: Term -> Term -> UnificationResult
unifyTerms term1 term2 =
    let runUnification :: UnificationState -> UnificationResult
        runUnification =
            fromEither
                . runExcept
                . fmap (UnificationSuccess . uSubstitution)
                . execStateT unification
        targets = freeVariables term1
     in runUnification $ State Map.empty targets [(term1, term2)]

data UnificationState
    = State
      { uSubstitution :: Substitution
      , uTargetVars :: Set VarName
      , uProblems :: [(Term, Term)]
      }

unification :: StateT UnificationState (Except UnificationResult) ()
unification = do
    mbNext <- gets $ listToMaybe . uProblems
    case mbNext of
        Nothing -> pure () -- done
        Just (term1, term2) -> do
            undefined term1 term2
    lift $ throwE UnificationFailed

--unify1 ::

---- TODO TODO TODO ------------------------------------

-- in Pattern.Util, and maybe in a CoFree soon?
freeVariables :: Term -> Set VarName
freeVariables = undefined
