{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.Pattern.Implication (
    simplifyImplication,
    ImplicationResult (..),
    ImplicationInvalidReason (..),
    ImplicationUnknownReason (..),
) where

import Booster.Definition.Base (KoreDefinition)
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base (Pattern (..), Predicate, Term)
import Booster.Pattern.Match qualified as Match
import Booster.Pattern.Unify (Substitution)

data ImplicationResult
    = ImplicationValid Substitution
    | ImplicationInvalid (Maybe Substitution) ImplicationInvalidReason
    | ImplicationUnknown (Maybe Substitution) ImplicationUnknownReason
    deriving stock (Eq, Show)

data ImplicationInvalidReason
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
        Match.MatchSuccess subst ->
            -- got substitution, let's now look at constraints
            case (antecedent.constraints, consequent.constraints) of
                -- successful matching
                ([], []) -> ImplicationValid subst
                (_, _) ->
                    ImplicationUnknown (Just subst) $
                        ConstraintSubsumptionUnknown (antecedent.constraints <> consequent.constraints)
        Match.MatchFailed reason -> ImplicationInvalid Nothing $ MatchingFailed reason
        Match.MatchIndeterminate antecedentSubterm consequentSubterm ->
            ImplicationUnknown Nothing $ MatchingUnknown antecedentSubterm consequentSubterm
