{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.Pattern.Implication (
    checkImplication,
    ImplicationCheckResult (..),
) where

import Booster.Definition.Base (KoreDefinition)
import Booster.Pattern.Base (Pattern (..), Predicate)
import Booster.Pattern.Unify qualified as Unify

data ImplicationCheckResult
    = ImplicationValid Unify.Substitution
    | ImplicationUnknown (Maybe Unify.FailReason) [Predicate]
    | ImplicationSortError Unify.SortError

-- | Attempt to check the implication between two patterns using matching and simplification rules
checkImplication :: KoreDefinition -> Pattern -> Pattern -> ImplicationCheckResult
checkImplication def antecedent consequent =
    case Unify.unifyTerms def consequent.term antecedent.term of
        Unify.UnificationSuccess subst ->
            -- got substitution, let's now look at constraints
            case (antecedent.constraints, consequent.constraints) of
                ([], []) -> ImplicationValid subst
                (_, _) -> ImplicationUnknown Nothing (antecedent.constraints <> consequent.constraints)
        Unify.UnificationFailed reason -> ImplicationUnknown (Just reason) []
        Unify.UnificationSortError sortError -> ImplicationSortError sortError
        Unify.UnificationRemainder pairs -> error (show pairs)
