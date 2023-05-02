{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.ApplyEquations (
    evaluateTerm,
    Direction (..),
    EquationPreference (..),
    EquationFailure (..),
    EquationTrace (..),
    ApplyEquationResult (..),
    isMatchFailure,
    isSuccess,
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Sequence (Seq (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter

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
    | TooManyIterations Int Term Term
    | EquationLoop [EquationTrace] [Term]
    | InternalError Text
    deriving stock (Eq, Show)

data EquationState = EquationState
    { definition :: KoreDefinition
    , llvmApi :: Maybe LLVM.API
    , termStack :: [Term]
    , changed :: Bool
    , trace :: Seq EquationTrace
    }

data EquationTrace = EquationTrace
    { subjectTerm :: Term
    , location :: Maybe Location
    , label :: Maybe Label
    , result :: ApplyEquationResult
    }
    deriving stock (Eq, Show)

instance Pretty EquationTrace where
    pretty EquationTrace{subjectTerm, location, label, result} = case result of
        Success rewritten ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "to"
                , pretty (PrettyTerm rewritten)
                , "using " <> locationInfo
                ]
        FailedMatch _ ->
            vsep ["Term did not match rule " <> locationInfo, prettyTerm]
        IndeterminateMatch ->
            vsep ["Term had indeterminate match for rule " <> locationInfo, prettyTerm]
        RuleNotPreservingDefinedness ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed because the rule at"
                , locationInfo
                , "does not preserve definedness"
                ]
        IndeterminateCondition ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed with indeterminate condition"
                , "using " <> locationInfo
                ]
        ConditionFalse ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed with false condition"
                , "using " <> locationInfo
                ]
        MatchConstraintViolated constrained varName ->
            pretty $
                "Concreteness constraint violated: "
                    <> show constrained
                    <> " variable "
                    <> show varName
      where
        locationInfo = pretty location <> " - " <> pretty label
        prettyTerm = pretty $ PrettyTerm subjectTerm

isMatchFailure, isSuccess :: EquationTrace -> Bool
isMatchFailure EquationTrace{result = FailedMatch{}} = True
isMatchFailure EquationTrace{result = IndeterminateMatch{}} = True
isMatchFailure _ = False
isSuccess EquationTrace{result = Success{}} = True
isSuccess _ = False

startState :: KoreDefinition -> Maybe LLVM.API -> EquationState
startState definition llvmApi =
    EquationState{definition, llvmApi, termStack = [], changed = False, trace = mempty}

getState :: EquationM EquationState
getState = EquationM get

countSteps :: EquationM Int
countSteps = length . (.termStack) <$> getState

pushTerm :: Term -> EquationM ()
pushTerm t = EquationM . modify $ \s -> s{termStack = t : s.termStack}

setChanged, resetChanged :: EquationM ()
setChanged = EquationM . modify $ \s -> s{changed = True}
resetChanged = EquationM . modify $ \s -> s{changed = False}

getChanged :: EquationM Bool
getChanged = EquationM $ gets (.changed)

checkForLoop :: Term -> EquationM ()
checkForLoop t = do
    EquationState{termStack, trace} <- getState
    whenJust (elemIndex t termStack) $ \i -> do
        throw (EquationLoop (toList trace) . reverse $ t : take (i + 1) termStack)

data Direction = TopDown | BottomUp
    deriving stock (Eq, Show)

data EquationPreference = PreferFunctions | PreferSimplifications
    deriving stock (Eq, Show)

runEquationM ::
    KoreDefinition ->
    Maybe LLVM.API ->
    EquationM a ->
    Either EquationFailure (a, [EquationTrace])
runEquationM definition llvmApi (EquationM m) =
    fmap (fmap $ toList . trace) <$> runExcept $ runStateT m $ startState definition llvmApi

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
            -- evaluate functions and simplify (recursively at each level)
            newTerm <- applyTerm direction preference currentTerm
            changeFlag <- getChanged
            if changeFlag
                then checkForLoop newTerm >> resetChanged >> go newTerm
                else pure currentTerm

----------------------------------------
-- Interface function
evaluateTerm ::
    Direction ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Term ->
    Either EquationFailure (Term, [EquationTrace])
evaluateTerm direction def llvmApi =
    runEquationM def llvmApi
        . iterateEquations 100 direction PreferFunctions

----------------------------------------

{- | Apply function equations and simplifications at all levels of a
   term AST, in the given direction (bottom-up or top-down).

  No iteration happens at the same AST level inside these traversals,
  one equation will be applied per level (if any).
-}
applyTerm ::
    Direction ->
    EquationPreference ->
    Term ->
    EquationM Term
applyTerm BottomUp pref =
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
            applyAtTop pref t
applyTerm TopDown pref = \t@(Term attributes _) ->
    if attributes.isEvaluated
        then pure t
        else do
            s <- getState
            -- All fully concrete values go to the LLVM backend (top-down only)
            if isConcrete t && isJust s.llvmApi
                then do
                    let result = simplifyTerm (fromJust s.llvmApi) s.definition t (sortOfTerm t)
                    when (result /= t) setChanged
                    pure result
                else apply t
  where
    apply = \case
        dv@DomainValue{} ->
            pure dv
        v@Var{} ->
            pure v
        Injection src trg t ->
            Injection src trg <$> applyTerm TopDown pref t -- no injection simplification
        AndTerm arg1 arg2 ->
            AndTerm -- no \and simplification
                <$> applyTerm TopDown pref arg1
                <*> applyTerm TopDown pref arg2
        app@(SymbolApplication sym sorts args) -> do
            -- try to apply equations
            t <- applyAtTop pref app
            if t /= app
                then do
                    case t of
                        SymbolApplication sym' sorts' args' ->
                            SymbolApplication sym' sorts'
                                <$> mapM (applyTerm TopDown pref) args'
                        _otherwise ->
                            applyTerm TopDown pref t -- won't loop
                else
                    SymbolApplication sym sorts
                        <$> mapM (applyTerm TopDown pref) args

{- | Try to apply function equations and simplifications to the given
   top-level term, in priority order and per group.
-}
applyAtTop ::
    EquationPreference ->
    Term ->
    EquationM Term
applyAtTop pref term = do
    def <- (.definition) <$> getState
    case pref of
        PreferFunctions -> do
            -- when applying equations, we want to catch DoesNotPreserveDefinedness/incosistentmatch/etc
            -- to do with functions, so as not to abort the entire simplification run
            fromFunctions <- applyEquations def.functionEquations handleFunctionEquation term
            if fromFunctions == term
                then applyEquations def.simplifications handleSimplificationEquation term
                else pure fromFunctions
        PreferSimplifications -> do
            simplified <- applyEquations def.simplifications handleSimplificationEquation term
            if simplified == term
                then applyEquations def.functionEquations handleFunctionEquation term
                else pure simplified

data ApplyEquationResult
    = Success Term
    | FailedMatch MatchFailReason
    | IndeterminateMatch
    | IndeterminateCondition
    | ConditionFalse
    | RuleNotPreservingDefinedness
    | MatchConstraintViolated Constrained VarName
    deriving stock (Eq, Show)

type ResultHandler =
    -- | action on successful equation application
    (Term -> EquationM Term) ->
    -- | action on failed match
    EquationM Term ->
    -- | action on aborted equation application
    EquationM Term ->
    ApplyEquationResult ->
    EquationM Term

handleFunctionEquation :: ResultHandler
handleFunctionEquation success continue abort = \case
    Success rewritten -> success rewritten
    FailedMatch _ -> continue
    IndeterminateMatch -> abort
    IndeterminateCondition -> abort
    ConditionFalse -> continue
    RuleNotPreservingDefinedness -> abort
    MatchConstraintViolated{} -> continue

handleSimplificationEquation :: ResultHandler
handleSimplificationEquation success continue _abort = \case
    Success rewritten -> success rewritten
    FailedMatch _ -> continue
    IndeterminateMatch -> continue
    IndeterminateCondition -> continue
    ConditionFalse -> continue
    RuleNotPreservingDefinedness -> continue
    MatchConstraintViolated{} -> continue

applyEquations ::
    forall tag.
    Theory (RewriteRule tag) ->
    ResultHandler ->
    Term ->
    EquationM Term
applyEquations theory handler term = do
    let index = termTopIndex term
    when (index == None) $
        throw (IndexIsNone term)
    let idxEquations, anyEquations :: Map Priority [RewriteRule tag]
        idxEquations = fromMaybe Map.empty $ Map.lookup index theory
        anyEquations = fromMaybe Map.empty $ Map.lookup Anything theory
        -- neither simplification nor function equations should need groups,
        -- since simplification priority is just a suggestion and function equations
        -- should not contain non-determinism except for the [owise] equation,
        -- which should be attempted last. Thus, sorting and then applying sequentially is fine.
        -- Doing this loses the runtime check of InconsistentFunctionRules, however,
        -- most function rules are in the same priority group and thus,
        -- we would be applying all of them before checking for inconsistency,
        -- which is inefficient
        equations :: [RewriteRule tag]
        equations =
            concatMap snd . Map.toAscList $
                if index == Anything
                    then idxEquations
                    else Map.unionWith (<>) idxEquations anyEquations

    processEquations equations
  where
    -- process one equation at a time, until something has happened
    processEquations ::
        [RewriteRule tag] ->
        EquationM Term
    processEquations [] =
        pure term -- nothing to do, term stays the same
    processEquations (eq : rest) = do
        res <- applyEquation term eq
        traceRuleApplication term eq.attributes.location eq.attributes.ruleLabel res
        handler (\t -> setChanged >> pure t) (processEquations rest) (pure term) res

traceRuleApplication ::
    Term ->
    Maybe Location ->
    Maybe Label ->
    ApplyEquationResult ->
    EquationM ()
traceRuleApplication t loc lbl res =
    EquationM . modify $
        \s -> s{trace = s.trace :|> EquationTrace t loc lbl res}

applyEquation ::
    forall tag.
    Term ->
    RewriteRule tag ->
    EquationM ApplyEquationResult
applyEquation term rule = fmap (either id Success) $ runExceptT $ do
    -- ensured by internalisation: no existentials in equations
    unless (null rule.existentials) $
        lift . throw . InternalError $
            "Equation with existentials: " <> Text.pack (show rule)
    -- immediately cancel if not preserving definedness
    unless (null rule.computedAttributes.notPreservesDefinednessReasons) $
        throwE RuleNotPreservingDefinedness
    -- immediately cancel if rule has concrete() flag and term has variables
    when (allMustBeConcrete rule.attributes.concreteness && not (Set.null (freeVariables term))) $
        throwE (MatchConstraintViolated Concrete "* (term has variables)")
    -- match lhs
    koreDef <- (.definition) <$> lift getState
    case matchTerm koreDef rule.lhs term of
        MatchFailed failReason -> throwE $ FailedMatch failReason
        MatchIndeterminate _pat _subj -> throwE IndeterminateMatch
        MatchSuccess subst -> do
            -- cancel if condition
            -- forall (v, t) : subst. concrete(v) -> isConstructorLike(t) /\
            --                        symbolic(v) -> not $ t isConstructorLike(t)
            -- is violated
            checkConcreteness rule.attributes.concreteness subst

            -- check conditions, using substitution (will call back
            -- into the simplifier! -> import loop)
            let newConstraints =
                    concatMap (splitBoolPredicates . substituteInPredicate subst) $
                        rule.requires
            unclearConditions' <- runMaybeT $ catMaybes <$> mapM checkConstraint newConstraints

            case unclearConditions' of
                Nothing -> throwE ConditionFalse
                Just unclearConditions ->
                    if not $ null unclearConditions
                        then throwE IndeterminateCondition
                        else do
                            let rewritten =
                                    substituteInTerm subst rule.rhs
                            -- NB no new constraints, as they have been checked to be `Top`
                            -- FIXME what about symbolic constraints here?
                            pure rewritten
  where
    -- evaluate/simplify a predicate, cut the operation short when it
    -- is Bottom.
    checkConstraint ::
        Predicate ->
        MaybeT (ExceptT ApplyEquationResult EquationM) (Maybe Predicate)
    checkConstraint p = do
        mApi <- (.llvmApi) <$> lift (lift getState)
        case simplifyPredicate mApi p of
            Bottom -> fail "side condition was false"
            Top -> pure Nothing
            _other -> pure $ Just p

    allMustBeConcrete (AllConstrained Concrete) = True
    allMustBeConcrete _ = False

    checkConcreteness ::
        Concreteness ->
        Map Variable Term ->
        ExceptT ApplyEquationResult EquationM ()
    checkConcreteness Unconstrained _ = pure ()
    checkConcreteness (AllConstrained constrained) subst =
        mapM_ (\(var, t) -> mkCheck (toPair var) constrained t) $ Map.assocs subst
    checkConcreteness (SomeConstrained mapping) subst =
        void $ Map.traverseWithKey (verifyVar subst) (Map.mapWithKey mkCheck mapping)

    toPair Variable{variableSort, variableName} =
        case variableSort of
            SortApp sortName _ -> (variableName, sortName)
            SortVar varName -> (variableName, varName)

    mkCheck ::
        (VarName, SortName) ->
        Constrained ->
        Term ->
        ExceptT ApplyEquationResult EquationM ()
    mkCheck (varName, _) constrained (Term attributes _)
        | not test = throwE $ MatchConstraintViolated constrained varName
        | otherwise = pure ()
      where
        test = case constrained of
            Concrete -> attributes.isConstructorLike
            Symbolic -> not attributes.isConstructorLike

    verifyVar ::
        Map Variable Term ->
        (VarName, SortName) ->
        (Term -> ExceptT ApplyEquationResult EquationM ()) ->
        ExceptT ApplyEquationResult EquationM ()
    verifyVar subst (variableName, sortName) check =
        maybe (error $ "Variable not found: " <> show (variableName, sortName)) check $
            Map.lookup Variable{variableSort = SortApp sortName [], variableName} subst

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
        let result = t -- FIXME simplify and evaluate here
        EquationM $ put prior
        pure result
