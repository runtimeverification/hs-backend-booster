{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Rewrite (
    performRewrite,
    rewriteStep,
    RewriteFailed (..),
    RuleFailed (..),
    RewriteResult (..),
) where

import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Except
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base
import Kore.Pattern.Simplify
import Kore.Pattern.Unify
import Kore.Pattern.Util

import Kore.Syntax.ParsedKore.Internalise (computeTermIndex) -- FIXME move this function!

{- | Performs a rewrite step (using suitable rewrite rules from the
   definition).

  The result can be a failure (providing some context for why it
  failed), or a rewritten pattern with a new term and possibly new
  additional constraints.
-}
rewriteStep :: KoreDefinition -> [Text] -> [Text] -> Pattern -> Except RewriteFailed RewriteResult
rewriteStep def cutLabels terminalLabels pat = do
    let termIdx = computeTermIndex pat.term
    when (termIdx == None) $ throwE TermIndexIsNone

    let idxRules = fromMaybe Map.empty $ Map.lookup termIdx def.rewriteTheory
        anyRules = fromMaybe Map.empty $ Map.lookup Anything def.rewriteTheory
        rules = map snd . Map.toAscList $ Map.unionWith (<>) idxRules anyRules

    when (null rules) $ throwE NoRulesForTerm

    -- process one priority group at a time (descending priority),
    -- until a result is obtained or the entire rewrite fails.
    processGroups rules
  where
    processGroups :: [[RewriteRule]] -> Except RewriteFailed RewriteResult
    processGroups [] =
        pure $ RewriteStuck pat
    processGroups (rules : rest) = do
        -- try all rules of the priority group
        let (failures, results) =
                partitionEithers $ map (runExcept . applyRule def pat) rules

        -- if any rule failure is an uncertainty, fail the rewrite
        -- immediately
        let uncertains = filter isUncertain failures
        unless (null uncertains) $
            throwE $
                RuleApplicationUncertain uncertains

        -- if any of the results does not preserve definedness, fail
        -- the rewrite immediately
        let maybeUndefined =
                filter
                    (not . (.computedAttributes.preservesDefinedness) . fst)
                    results
        unless (null maybeUndefined) $
            throwE $
                DefinednessUnclear maybeUndefined

        -- simplify and filter out bottom states
        let finalResults = filter (not . isBottom . simplifyPattern . snd) results

        let hasLabelIn :: RewriteRule -> [Text] -> Bool
            hasLabelIn = elem . fromMaybe "" . (.ruleLabel) . (.attributes)
        case finalResults of
            [] ->
                processGroups rest
            [(r, x)]
                | r `hasLabelIn` cutLabels ->
                    pure $ RewriteCutPoint pat x
                | r `hasLabelIn` terminalLabels ->
                    pure $ RewriteTerminal x
                | otherwise ->
                    pure $ RewriteSingle x
            rxs ->
                pure $ RewriteBranch $ NE.fromList $ map snd rxs

{- | Tries to apply one rewrite rule:

 * Unifies the LHS term with the pattern term
 * Ensures that the unification is a _match_ (one-sided substitution)
 * prunes any rules that turn out to have trivially-false side conditions
 * returns the rule and the resulting pattern
-}
applyRule ::
    KoreDefinition ->
    Pattern ->
    RewriteRule ->
    Except RuleFailed (RewriteRule, Pattern)
applyRule def pat rule = do
    -- unify terms
    let unified = unifyTerms def rule.lhs.term pat.term
    subst <- case unified of
        UnificationFailed reason ->
            throwE $ RewriteUnificationFailed reason
        UnificationSortError sortError ->
            throwE $ RewriteSortError sortError
        UnificationRemainder remainder ->
            throwE $ RewriteUnificationUnclear rule remainder
        InternalError msg ->
            throwE $ RewriteRuleInternalError msg
        UnificationSuccess substitution ->
            pure substitution

    -- check it is a matching substitution (stop if not)
    unless (Map.keysSet subst == freeVariables rule.lhs.term) $
        throwE $
            UnificationIsNotMatch rule subst

    -- apply substitution to rule constraints and simplify (stop if
    -- false, one by one in isolation)
    let newConstraints =
            map (substituteInPredicate subst) $
                rule.lhs.constraints <> rule.rhs.constraints
    mapM_ checkConstraint newConstraints

    let rewritten =
            Pattern
                (substituteInTerm subst rule.rhs.term)
                (map (substituteInPredicate subst) $ pat.constraints <> newConstraints)
    return (rule, rewritten)
  where
    checkConstraint :: Predicate -> Except RuleFailed ()
    checkConstraint p =
        when (simplifyPredicate p == Bottom) $
            throwE $
                ConstraintIsBottom p

{- | Reason why a rewrite did not produce a result. Contains additional
   information for logging what happened during the rewrite.
-}
data RewriteFailed
    = -- | No rules have been found
      NoRulesForTerm
    | -- | All rules have been tried unsuccessfully
      NoApplicableRules
    | -- | It is uncertain whether or not rules can be applied
      RuleApplicationUncertain [RuleFailed]
    | -- | There are rewrites that do not preserve definedness
      DefinednessUnclear [(RewriteRule, Pattern)]
    | -- | Term has index 'None', no rule should apply
      TermIndexIsNone
    deriving stock (Eq, Show)

data RuleFailed
    = -- | The rule's LHS term and the pattern term do not unify at all
      RewriteUnificationFailed FailReason
    | -- | The rule's LHS term and the pattern term do not unify with certainty
      RewriteUnificationUnclear RewriteRule (NonEmpty (Term, Term))
    | -- | A sort error occurred during unification
      RewriteSortError SortError
    | -- | The unification did not produce a matching substitution
      UnificationIsNotMatch RewriteRule Substitution
    | -- | A constraint of the rule simplifies to Bottom (when substituted)
      ConstraintIsBottom Predicate
    | -- | Internal error (from unification or elsewhere)
      RewriteRuleInternalError String
    deriving stock (Eq, Show)

isUncertain :: RuleFailed -> Bool
isUncertain RewriteUnificationFailed{} = False
isUncertain RewriteUnificationUnclear{} = True
isUncertain RewriteSortError{} = True
isUncertain UnificationIsNotMatch{} = True
isUncertain ConstraintIsBottom{} = False
isUncertain RewriteRuleInternalError{} = True

-- | Different rewrite results (returned from RPC execute endpoint)
data RewriteResult
    = -- | single result (internal use, not returned)
      RewriteSingle Pattern
    | -- | branch point
      RewriteBranch (NonEmpty Pattern)
    | -- | no rules could be applied, config is stuck
      RewriteStuck Pattern
    | -- | cut point rule, return current (lhs) and single next state
      RewriteCutPoint Pattern Pattern
    | -- | terminal rule, return rhs (final state reached)
      RewriteTerminal Pattern
    | -- | stopping because maximum depth has been reached
      RewriteStopped Pattern
    | -- | unable to handle the current case with this rewriter
      -- (signalled by exceptions)
      RewriteAborted Pattern
    deriving stock (Eq, Show)

{- | Interface for RPC execute: Rewrite given term as long as there is
   exactly one result in each step.

  * multiple results: a branch point, return current and all results
  * RewriteStuck: config simplified to #Bottom, return current as stuck
  * RewriteCutPoint: a cut-point rule was applied, return lhs and rhs
  * RewriteTerminal: a terminal rule was applied, return rhs

  * RewriteFailed: rewriter cannot handle the case, return current

  The actions are logged at the custom log level '"Rewrite"'.
-}
performRewrite ::
    forall io.
    MonadLoggerIO io =>
    KoreDefinition ->
    -- | maximum depth
    Maybe Int ->
    -- | cut point rule labels
    [Text] ->
    -- | terminal rule labels
    [Text] ->
    Pattern ->
    io RewriteResult
performRewrite def mbMaxDepth cutLabels terminalLabels pat = do
    logRewrite $ "Rewriting pattern " <> pack (show pat)
    doSteps 1 pat
  where
    logRewrite = logOther (LevelOther "Rewrite")

    depthReached n = maybe False (n >) mbMaxDepth

    showCounter = (<> " steps.") . pack . show

    doSteps :: Int -> Pattern -> io RewriteResult
    doSteps counter pat'
        | depthReached counter = do
            logRewrite $ "Reached maximum depth of " <> maybe "?" showCounter mbMaxDepth
            pure $ RewriteStopped pat'
        | otherwise = do
            let res = runExcept $ rewriteStep def cutLabels terminalLabels pat'
            case res of
                Right (RewriteSingle single) ->
                    doSteps (counter + 1) single
                Right other -> do
                    logRewrite $ "Stopped after " <> showCounter counter
                    logRewrite $ pack (show other)
                    pure other
                Left failure -> do
                    logRewrite $ "Aborted after " <> showCounter counter
                    logRewrite $ pack (show failure)
                    pure $ RewriteAborted pat'
