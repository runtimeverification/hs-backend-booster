{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.Pattern.Implication (
    simplifyImplication,
    ImplicationResult (..),
) where

import Booster.Definition.Base (KoreDefinition)
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base (Pattern (..), Predicate, Term)
import Booster.Pattern.Match qualified as Match

-- | Result of matching a pattern to a subject (unification, failure, or indeterminate)
data ImplicationResult
    = ImplicationValid
    | ImplicationInvalid ImplicationFailureReason
    | ImplicationUnknown ImplicationUnknownReason
    deriving stock (Eq, Show)

data ImplicationFailureReason
    = MatchingFailed Match.MatchFailReason
    | ConstraintSubsumptionFailed [Predicate]
    deriving stock (Eq, Show)

data ImplicationUnknownReason
    = MatchingUnknown Term Term
    | ConstraintSubsumptionUnknown [Predicate]
    deriving stock (Eq, Show)

-- | Check implication between two patterns using matching and simplification rules
simplifyImplication ::
    Bool ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Pattern ->
    Pattern ->
    ImplicationResult
simplifyImplication _doTracing def _mLlvmLibrary antecedent consequent =
    case Match.matchTerm def antecedent.term consequent.term of
        Match.MatchSuccess _subst ->
            -- got substitution, let's now look at constraints
            case (antecedent.constraints, consequent.constraints) of
                ([], []) -> ImplicationValid
                (_, _) ->
                    ImplicationUnknown $ ConstraintSubsumptionUnknown (antecedent.constraints <> consequent.constraints)
        Match.MatchFailed reason -> ImplicationInvalid . MatchingFailed $ reason
        Match.MatchIndeterminate antecedent_term consequent_term -> ImplicationUnknown $ MatchingUnknown antecedent_term consequent_term
