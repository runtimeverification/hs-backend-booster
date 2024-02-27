{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.ApplyEquations (
    evaluateTerm,
    evaluatePattern,
    Direction (..),
    EquationT (..),
    runEquationT,
    EquationConfig (..),
    getConfig,
    EquationPreference (..),
    EquationFailure (..),
    EquationTrace (..),
    pattern CollectEquationTraces,
    pattern NoCollectEquationTraces,
    eraseStates,
    EquationMetadata (..),
    ApplyEquationResult (..),
    ApplyEquationFailure (..),
    applyEquations,
    handleSimplificationEquation,
    isMatchFailure,
    isSuccess,
    simplifyConstraint,
    simplifyConstraints,
    SimplifierCache,
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.CallStack (
    LogLevel (..),
    MonadLogger,
    MonadLoggerIO,
    logOther,
    logOtherNS,
 )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.State
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (toList, traverse_)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Sequence (Seq (..), pattern (:<|))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Prettyprinter

import Booster.Builtin as Builtin
import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.GlobalState qualified as GlobalState
import Booster.LLVM qualified as LLVM
import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Index
import Booster.Pattern.Match
import Booster.Pattern.Util
import Booster.Prettyprinter (renderDefault, renderText)
import Booster.SMT.Interface qualified as SMT
import Booster.Util (Bound (..), Flag (..))

newtype EquationT io a
    = EquationT (ReaderT EquationConfig (ExceptT EquationFailure (StateT EquationState io)) a)
    -- ~ EquationConfig -> EquationState -> io (Either EquationFailure a, EquationState)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

throw :: MonadLoggerIO io => EquationFailure -> EquationT io a
throw = EquationT . lift . throwE

catch_ ::
    MonadLoggerIO io => EquationT io a -> (EquationFailure -> EquationT io a) -> EquationT io a
catch_ (EquationT op) hdlr = EquationT $ do
    cfg <- ask
    lift (runReaderT op cfg `catchE` (\e -> let EquationT fallBack = hdlr e in runReaderT fallBack cfg))

data EquationFailure
    = IndexIsNone Term
    | TooManyIterations Int Term Term
    | EquationLoop [Term]
    | TooManyRecursions [Term]
    | SideConditionFalse Predicate
    | InternalError Text
    | UndefinedTerm Term LLVM.LlvmError
    deriving stock (Eq, Show, Data)

instance Pretty EquationFailure where
    pretty = \case
        IndexIsNone t ->
            "Index 'None' for term " <> pretty t
        TooManyIterations count start end ->
            vsep
                [ "Unable to finish evaluation in " <> pretty count <> " iterations"
                , "Started with: " <> pretty start
                , "Stopped at: " <> pretty end
                ]
        EquationLoop ts ->
            vsep $ "Evaluation produced a loop:" : map pretty ts
        TooManyRecursions ts ->
            vsep $
                "Recursion limit exceeded. The following terms were evaluated:"
                    : map pretty ts
        SideConditionFalse p ->
            vsep
                [ "A side condition was found to be false during evaluation (pruning)"
                , pretty p
                ]
        UndefinedTerm t (LLVM.LlvmError err) ->
            vsep
                [ "Term"
                , pretty t
                , "is undefined: "
                , pretty (BS.unpack err)
                ]
        InternalError msg ->
            "Internal error during evaluation: " <> pretty msg

data EquationConfig = EquationConfig
    { definition :: KoreDefinition
    , llvmApi :: Maybe LLVM.API
    , smtSolver :: Maybe SMT.SMTContext
    , doTracing :: Flag "CollectEquationTraces"
    , maxRecursion :: Bound "Recursion"
    , maxIterations :: Bound "Iterations"
    }

pattern CollectEquationTraces :: Flag "CollectEquationTraces"
pattern CollectEquationTraces = Flag True

pattern NoCollectEquationTraces :: Flag "CollectEquationTraces"
pattern NoCollectEquationTraces = Flag False

data EquationState = EquationState
    { termStack :: Seq Term
    , recursionStack :: [Term]
    , changed :: Bool
    , predicates :: Set Predicate
    , trace :: Seq (EquationTrace Term)
    , cache :: SimplifierCache
    }

type SimplifierCache = Map Term Term

data EquationMetadata = EquationMetadata
    { location :: Maybe Location
    , label :: Maybe Label
    , ruleId :: Maybe UniqueId
    }
    deriving stock (Eq, Show)

data EquationTrace term
    = EquationApplied term EquationMetadata term
    | EquationNotApplied term EquationMetadata ApplyEquationFailure
    deriving stock (Eq, Show)

{- | For the given equation trace, construct a new one,
     removing the heavy-weight information (the states),
     but keeping the meta-data (rule labels).
-}
eraseStates :: EquationTrace Term -> EquationTrace ()
eraseStates = \case
    EquationApplied _ metadata _ -> EquationApplied () metadata ()
    EquationNotApplied _ metadata failureInfo -> EquationNotApplied () metadata failureInfo

instance Pretty (EquationTrace Term) where
    pretty (EquationApplied subjectTerm metadata rewritten) =
        vsep
            [ "Simplifying term"
            , prettyTerm
            , "to"
            , pretty rewritten
            , "using " <> locationInfo
            ]
      where
        locationInfo = pretty metadata.location <> " - " <> pretty metadata.label
        prettyTerm = pretty subjectTerm
    pretty (EquationNotApplied subjectTerm metadata result) = case result of
        FailedMatch reason ->
            vsep ["Term did not match rule " <> locationInfo, prettyTerm, pretty reason]
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
        IndeterminateCondition cs ->
            vsep $
                [ "Simplifying term"
                , prettyTerm
                , "failed with indeterminate condition(s):"
                ]
                    ++ map pretty cs
                    ++ ["using " <> locationInfo]
        ConditionFalse p ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed with false condition"
                , pretty p
                , "using " <> locationInfo
                ]
        EnsuresFalse p ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "using " <> locationInfo
                , "resulted in ensuring false condition"
                , pretty p
                ]
        MatchConstraintViolated constrained varName ->
            vsep
                [ "Concreteness constraint violated: "
                , pretty $ show constrained <> " variable " <> show varName
                , " in rule " <> locationInfo
                , "Term:"
                , prettyTerm
                ]
      where
        locationInfo = pretty metadata.location <> " - " <> pretty metadata.label
        prettyTerm = pretty subjectTerm

isMatchFailure, isSuccess :: EquationTrace Term -> Bool
isMatchFailure (EquationNotApplied _ _ FailedMatch{}) = True
isMatchFailure (EquationNotApplied _ _ IndeterminateMatch{}) = True
isMatchFailure _ = False
isSuccess EquationApplied{} = True
isSuccess _ = False

startState :: Map Term Term -> EquationState
startState cache =
    EquationState
        { termStack = mempty
        , recursionStack = []
        , changed = False
        , predicates = mempty
        , trace = mempty
        , cache
        }

eqState :: MonadLoggerIO io => StateT EquationState io a -> EquationT io a
eqState = EquationT . lift . lift

getState :: MonadLoggerIO io => EquationT io EquationState
getState = eqState get

getConfig :: MonadLoggerIO io => EquationT io EquationConfig
getConfig = EquationT ask

countSteps :: MonadLoggerIO io => EquationT io Int
countSteps = length . (.termStack) <$> getState

pushTerm :: MonadLoggerIO io => Term -> EquationT io ()
pushTerm t = eqState . modify $ \s -> s{termStack = t :<| s.termStack}

pushConstraints :: MonadLoggerIO io => Set Predicate -> EquationT io ()
pushConstraints ps = eqState . modify $ \s -> s{predicates = s.predicates <> ps}

setChanged, resetChanged :: MonadLoggerIO io => EquationT io ()
setChanged = eqState . modify $ \s -> s{changed = True}
resetChanged = eqState . modify $ \s -> s{changed = False}

getChanged :: MonadLoggerIO io => EquationT io Bool
getChanged = eqState $ gets (.changed)

pushRecursion :: MonadLoggerIO io => Term -> EquationT io (Bound "Recursion")
pushRecursion t = eqState $ do
    stk <- gets (.recursionStack)
    modify $ \s -> s{recursionStack = t : stk}
    pure (coerce $ 1 + length stk)

popRecursion :: MonadLoggerIO io => EquationT io ()
popRecursion = do
    s <- getState
    if null s.recursionStack
        then throw $ InternalError "Trying to pop an empty recursion stack"
        else eqState $ put s{recursionStack = tail s.recursionStack}

toCache :: MonadLoggerIO io => Term -> Term -> EquationT io ()
toCache orig result = eqState . modify $ \s -> s{cache = Map.insert orig result s.cache}

fromCache :: MonadLoggerIO io => Term -> EquationT io (Maybe Term)
fromCache t = eqState $ Map.lookup t <$> gets (.cache)

checkForLoop :: MonadLoggerIO io => Term -> EquationT io ()
checkForLoop t = do
    EquationState{termStack} <- getState
    whenJust (Seq.elemIndexL t termStack) $ \i -> do
        throw (EquationLoop $ reverse $ t : take (i + 1) (toList termStack))

data Direction = TopDown | BottomUp
    deriving stock (Eq, Show)

data EquationPreference = PreferFunctions | PreferSimplifications
    deriving stock (Eq, Show)

runEquationT ::
    MonadLoggerIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    EquationT io a ->
    io (Either EquationFailure a, [EquationTrace Term], SimplifierCache)
runEquationT doTracing definition llvmApi smtSolver sCache (EquationT m) = do
    globalEquationOptions <- liftIO GlobalState.readGlobalEquationOptions
    (res, endState) <-
        flip runStateT (startState sCache) $
            runExceptT $
                runReaderT
                    m
                    EquationConfig
                        { definition
                        , llvmApi
                        , smtSolver
                        , doTracing
                        , maxIterations = globalEquationOptions.maxIterations
                        , maxRecursion = globalEquationOptions.maxRecursion
                        }
    pure (res, toList endState.trace, endState.cache)

iterateEquations ::
    MonadLoggerIO io =>
    Direction ->
    EquationPreference ->
    Term ->
    EquationT io Term
iterateEquations direction preference startTerm = do
    logOther (LevelOther "Simplify") $ "Evaluating " <> renderText (pretty startTerm)
    result <- pushRecursion startTerm >>= checkCounter >> go startTerm <* popRecursion
    logOther (LevelOther "Simplify") $ "Result: " <> renderText (pretty result)
    pure result
  where
    checkCounter counter = do
        config <- getConfig
        when (counter > config.maxRecursion) $
            throw . TooManyRecursions . (.recursionStack) =<< getState

    go :: MonadLoggerIO io => Term -> EquationT io Term
    go currentTerm
        | (getAttributes currentTerm).isEvaluated = pure currentTerm
        | otherwise = do
            config <- getConfig
            currentCount <- countSteps
            when (coerce currentCount > config.maxIterations) $
                throw $
                    TooManyIterations currentCount startTerm currentTerm
            pushTerm currentTerm
            -- simplify the term using the LLVM backend first
            llvmResult <- llvmSimplify currentTerm
            -- NB llvmSimplify is idempotent. No need to iterate if
            -- the equation evaluation does not change the term any more.
            resetChanged
            -- evaluate functions and simplify (recursively at each level)
            newTerm <- applyTerm direction preference llvmResult
            changeFlag <- getChanged
            if changeFlag
                then checkForLoop newTerm >> resetChanged >> go newTerm
                else pure currentTerm

llvmSimplify :: forall io. MonadLoggerIO io => Term -> EquationT io Term
llvmSimplify t = do
    config <- getConfig
    case config.llvmApi of
        Nothing -> pure t
        Just api -> do
            logOtherNS "booster" (LevelOther "Simplify") "Calling LLVM simplification"
            llvmEval config.definition api t

llvmEval :: MonadLoggerIO io => KoreDefinition -> LLVM.API -> Term -> EquationT io Term
llvmEval definition api = eval
  where
    eval t@(Term attributes _)
        | attributes.isEvaluated = pure t
        | isConcrete t && attributes.canBeEvaluated = do
            LLVM.simplifyTerm api definition t (sortOfTerm t)
                >>= \case
                    Left e -> throw $ UndefinedTerm t e
                    Right result -> do
                        when (result /= t) $ do
                            setChanged
                            emitEquationTrace t Nothing (Just "LLVM") Nothing $ Success result
                        toCache t result
                        pure result
        | otherwise = do
            result <- descend t
            toCache t result
            pure result
    descend = \case
        dv@DomainValue{} -> pure dv
        v@Var{} -> pure v
        Injection src trg t ->
            Injection src trg <$> eval t -- no injection simplification
        AndTerm arg1 arg2 ->
            AndTerm -- no \and simplification
                <$> eval arg1
                <*> eval arg2
        SymbolApplication sym sorts args ->
            SymbolApplication sym sorts <$> mapM eval args
        KMap def keyVals rest ->
            KMap def
                <$> mapM (\(k, v) -> (,) <$> eval k <*> eval v) keyVals
                <*> maybe (pure Nothing) ((Just <$>) . eval) rest
        KList def heads rest ->
            KList def
                <$> mapM eval heads
                <*> maybe
                    (pure Nothing)
                    ( (Just <$>)
                        . \(mid, tails) ->
                            (,)
                                <$> eval mid
                                <*> mapM eval tails
                    )
                    rest
        KSet def keyVals rest ->
            KSet def
                <$> mapM eval keyVals
                <*> maybe (pure Nothing) ((Just <$>) . eval) rest

----------------------------------------
-- Interface functions

-- | Evaluate and simplify a term.
evaluateTerm ::
    MonadLoggerIO io =>
    Flag "CollectEquationTraces" ->
    Direction ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    Term ->
    io (Either EquationFailure Term, [EquationTrace Term], SimplifierCache)
evaluateTerm doTracing direction def llvmApi smtSolver =
    runEquationT doTracing def llvmApi smtSolver mempty
        . evaluateTerm' direction

-- version for internal nested evaluation
evaluateTerm' ::
    MonadLoggerIO io =>
    Direction ->
    Term ->
    EquationT io Term
evaluateTerm' direction = iterateEquations direction PreferFunctions

{- | Simplify a Pattern, processing its constraints independently.
     Returns either the first failure or the new pattern if no failure was encountered
-}
evaluatePattern ::
    MonadLoggerIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    Pattern ->
    io (Either EquationFailure Pattern, [EquationTrace Term], SimplifierCache)
evaluatePattern doTracing def mLlvmLibrary smtSolver cache =
    runEquationT doTracing def mLlvmLibrary smtSolver cache . evaluatePattern'

-- version for internal nested evaluation
evaluatePattern' ::
    MonadLoggerIO io =>
    Pattern ->
    EquationT io Pattern
evaluatePattern' Pattern{term, constraints, ceilConditions} = do
    pushConstraints constraints
    newTerm <- evaluateTerm' BottomUp term
    -- after evaluating the term, evaluate all (existing and
    -- newly-acquired) constraints, once
    traverse_ simplifyAssumedPredicate . predicates =<< getState
    -- this may yield additional new constraints, left unevaluated
    evaluatedConstraints <- predicates <$> getState
    pure Pattern{constraints = evaluatedConstraints, term = newTerm, ceilConditions}
  where
    -- evaluate the given predicate assuming all others
    simplifyAssumedPredicate p = do
        allPs <- predicates <$> getState
        let otherPs = Set.delete p allPs
        eqState $ modify $ \s -> s{predicates = otherPs}
        newP <- simplifyConstraint' True $ coerce p
        pushConstraints $ Set.singleton $ coerce newP

----------------------------------------

{- | Apply function equations and simplifications at all levels of a
   term AST, in the given direction (bottom-up or top-down).

  No iteration happens at the same AST level inside these traversals,
  one equation will be applied per level (if any).
-}
applyTerm ::
    MonadLoggerIO io =>
    Direction ->
    EquationPreference ->
    Term ->
    EquationT io Term
applyTerm direction pref trm = do
    config <- getConfig -- avoid re-reading config at every node
    descend config trm
  where
    -- descend :: EquationConfig -> Term -> EquationT io Term
    descend config t@(Term attributes _)
        | attributes.isEvaluated = pure t
        | otherwise =
            fromCache t >>= \case
                Nothing -> do
                    simplified <- apply config t
                    toCache t simplified
                    pure simplified
                Just cached -> do
                    when (t /= cached) $ do
                        setChanged
                        emitEquationTrace t Nothing (Just "Cache") Nothing $ Success cached
                    pure cached

    -- Bottom-up evaluation traverses AST nodes in post-order but finds work top-down
    -- Top-down evaluation traverses AST nodes in pre-order
    apply config = \case
        dv@DomainValue{} ->
            pure dv
        v@Var{} ->
            pure v
        Injection src trg t ->
            Injection src trg <$> descend config t -- no injection simplification
        AndTerm arg1 arg2 ->
            AndTerm -- no \and simplification
                <$> descend config arg1
                <*> descend config arg2
        app@(SymbolApplication sym sorts args)
            | direction == BottomUp -> do
                -- evaluate arguments first
                args' <- mapM (descend config) args
                -- then try to apply equations
                applyAtTop pref $ SymbolApplication sym sorts args'
            | otherwise {- direction == TopDown -} -> do
                -- try to apply equations
                t <- applyAtTop pref app
                -- then recurse into arguments or re-evaluate (avoiding a loop)
                if t /= app
                    then do
                        case t of
                            SymbolApplication sym' sorts' args' ->
                                SymbolApplication sym' sorts'
                                    <$> mapM (descend config) args'
                            _otherwise ->
                                descend config t -- won't loop
                    else
                        SymbolApplication sym sorts
                            <$> mapM (descend config) args
        -- maps, lists, and sets, are not currently evaluated because
        -- the matcher does not provide matches on collections.
        KMap def keyVals rest ->
            KMap def
                <$> mapM (\(k, v) -> (,) <$> descend config k <*> descend config v) keyVals
                <*> maybe (pure Nothing) ((Just <$>) . descend config) rest
        KList def heads rest ->
            KList def
                <$> mapM (descend config) heads
                <*> maybe
                    (pure Nothing)
                    ( (Just <$>)
                        . \(mid, tails) ->
                            (,)
                                <$> descend config mid
                                <*> mapM (descend config) tails
                    )
                    rest
        KSet def keyVals rest ->
            KSet def
                <$> mapM (descend config) keyVals
                <*> maybe (pure Nothing) ((Just <$>) . descend config) rest

{- | Try to apply function equations and simplifications to the given
   top-level term, in priority order and per group.
-}
applyAtTop ::
    forall io.
    MonadLoggerIO io =>
    EquationPreference ->
    Term ->
    EquationT io Term
applyAtTop pref term = do
    -- always try built-in (hooked) functions first, then equations
    fromMaybeM tryEquations tryBuiltins
  where
    tryBuiltins :: EquationT io (Maybe Term)
    tryBuiltins = do
        case term of
            SymbolApplication sym _sorts args
                | Just hook <- flip Map.lookup Builtin.hooks =<< sym.attributes.hook -> do
                    logOther (LevelOther "Simplify") $
                        "Calling hooked function "
                            <> (Text.decodeUtf8 $ fromJust sym.attributes.hook)
                            <> " for "
                            <> renderText (pretty term)
                    either (throw . InternalError) checkChanged
                        . runExcept
                        $ hook args
            _other -> pure Nothing

    -- for the (unlikely) case that a built-in reproduces itself, we
    -- cannot blindly set the changed flag when we have applied a hook
    checkChanged :: Maybe Term -> EquationT io (Maybe Term)
    checkChanged Nothing =
        logOther (LevelOther "Simplify") "Hook returned no result" >> pure Nothing
    checkChanged (Just t) = do
        logOther (LevelOther "Simplify") . renderText $
            "Hook returned " <> pretty t
        unless (t == term) setChanged
        pure (Just t)

    tryEquations :: EquationT io Term
    tryEquations = do
        def <- (.definition) <$> getConfig
        case pref of
            PreferFunctions -> do
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
    | Failure ApplyEquationFailure
    deriving stock (Eq, Show)

data ApplyEquationFailure
    = FailedMatch MatchFailReason
    | IndeterminateMatch
    | IndeterminateCondition [Predicate]
    | ConditionFalse Predicate
    | EnsuresFalse Predicate
    | RuleNotPreservingDefinedness
    | MatchConstraintViolated Constrained VarName
    deriving stock (Eq, Show)

type ResultHandler io =
    -- | action on successful equation application
    (Term -> EquationT io Term) ->
    -- | action on failed match
    EquationT io Term ->
    -- | action on aborted equation application
    EquationT io Term ->
    ApplyEquationResult ->
    EquationT io Term

handleFunctionEquation :: MonadLoggerIO io => ResultHandler io
handleFunctionEquation success continue abort = \case
    Success rewritten -> success rewritten
    Failure (FailedMatch _) -> continue
    Failure IndeterminateMatch -> abort
    Failure (IndeterminateCondition{}) -> abort
    Failure (ConditionFalse _) -> continue
    Failure (EnsuresFalse p) -> throw $ SideConditionFalse p
    Failure RuleNotPreservingDefinedness -> abort
    Failure (MatchConstraintViolated{}) -> continue

handleSimplificationEquation :: MonadLoggerIO io => ResultHandler io
handleSimplificationEquation success continue _abort = \case
    Success rewritten -> success rewritten
    Failure (FailedMatch _) -> continue
    Failure IndeterminateMatch -> continue
    Failure (IndeterminateCondition{}) -> continue
    Failure (ConditionFalse _) -> continue
    Failure (EnsuresFalse p) -> throw $ SideConditionFalse p
    Failure RuleNotPreservingDefinedness -> continue
    Failure (MatchConstraintViolated{}) -> continue

applyEquations ::
    forall io tag.
    MonadLoggerIO io =>
    Theory (RewriteRule tag) ->
    ResultHandler io ->
    Term ->
    EquationT io Term
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
        EquationT io Term
    processEquations [] =
        pure term -- nothing to do, term stays the same
    processEquations (eq : rest) = do
        res <- applyEquation term eq
        emitEquationTrace term eq.attributes.location eq.attributes.ruleLabel eq.attributes.uniqueId res
        handler (\t -> setChanged >> pure t) (processEquations rest) (pure term) res

{- | Trace application or failure to apply an equation
     * log into stderr
     * accumulate the trace into the state
-}
emitEquationTrace ::
    MonadLoggerIO io =>
    Term ->
    Maybe Location ->
    Maybe Label ->
    Maybe UniqueId ->
    ApplyEquationResult ->
    EquationT io ()
emitEquationTrace t loc lbl uid res = do
    let newTraceItem =
            case res of
                Success rewritten -> EquationApplied t (EquationMetadata loc lbl uid) rewritten
                Failure failure -> EquationNotApplied t (EquationMetadata loc lbl uid) failure
        prettyItem = pack . renderDefault . pretty $ newTraceItem
    logOther (LevelOther "Simplify") prettyItem
    case res of
        Success{} -> logOther (LevelOther "SimplifySuccess") prettyItem
        _ -> pure ()
    config <- getConfig
    when (coerce config.doTracing) $
        eqState . modify $
            \s -> s{trace = s.trace :|> newTraceItem}

applyEquation ::
    forall io tag.
    MonadLoggerIO io =>
    Term ->
    RewriteRule tag ->
    EquationT io ApplyEquationResult
applyEquation term rule = fmap (either Failure Success) $ runExceptT $ do
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
    koreDef <- (.definition) <$> lift getConfig
    case matchTerm koreDef rule.lhs term of
        MatchFailed failReason -> throwE $ FailedMatch failReason
        MatchIndeterminate _pat _subj -> throwE IndeterminateMatch
        MatchSuccess subst -> do
            -- cancel if condition
            -- forall (v, t) : subst. concrete(v) -> isConstructorLike(t) /\
            --                        symbolic(v) -> not $ t isConstructorLike(t)
            -- is violated
            checkConcreteness rule.attributes.concreteness subst

            -- check required conditions, using substitution
            let required =
                    concatMap
                        (splitBoolPredicates . coerce . substituteInTerm subst . coerce)
                        rule.requires
            -- If the required condition is _syntactically_ present in
            -- the prior (known constraints), we don't check it.
            knownPredicates <- (.predicates) <$> lift getState
            let (knownTrue, toCheck) = partition (`Set.member` knownPredicates) required
            unless (null knownTrue) $
                logOtherNS "booster" (LevelOther "Simplify") . renderText $
                    vsep ("Conditions known to be true: " : map pretty knownTrue)

            unclearConditions' <- catMaybes <$> mapM (checkConstraint ConditionFalse) toCheck

            case unclearConditions' of
                [] -> do
                    -- check ensured conditions, filter any
                    -- true ones, prune if any is false
                    let ensured =
                            concatMap
                                (splitBoolPredicates . substituteInPredicate subst)
                                (Set.toList rule.ensures)
                    ensuredConditions <-
                        -- throws if an ensured condition found to be false
                        catMaybes <$> mapM (checkConstraint EnsuresFalse) ensured
                    lift $ pushConstraints $ Set.fromList ensuredConditions
                    pure $ substituteInTerm subst rule.rhs
                unclearConditions -> throwE $ IndeterminateCondition unclearConditions
  where
    -- Simplify given predicate in a nested EquationT execution.
    -- Call 'whenBottom' if it is Bottom, return Nothing if it is Top,
    -- otherwise return the simplified remaining predicate.
    checkConstraint ::
        (Predicate -> ApplyEquationFailure) ->
        Predicate ->
        ExceptT ApplyEquationFailure (EquationT io) (Maybe Predicate)
    checkConstraint whenBottom (Predicate p) = do
        logOther (LevelOther "Simplify") $
            "Recursive simplification of predicate: " <> pack (renderDefault (pretty p))
        let fallBackToUnsimplifiedOrBottom :: EquationFailure -> EquationT io Term
            fallBackToUnsimplifiedOrBottom = \case
                e@UndefinedTerm{} -> do
                    logOther (LevelOther "Simplify") . pack . renderDefault $ pretty e
                    pure FalseBool
                e -> do
                    logOther (LevelOther "Simplify") . pack . renderDefault $
                        "Aborting recursive simplification:" <> pretty e
                    pure p
        -- exceptions need to be handled differently in the recursion,
        -- falling back to the unsimplified constraint instead of aborting.
        simplified <-
            lift $ simplifyConstraint' True p `catch_` fallBackToUnsimplifiedOrBottom
        case simplified of
            FalseBool -> throwE . whenBottom $ coerce p
            TrueBool -> pure Nothing
            other -> pure . Just $ coerce other

    allMustBeConcrete (AllConstrained Concrete) = True
    allMustBeConcrete _ = False

    checkConcreteness ::
        Concreteness ->
        Map Variable Term ->
        ExceptT ApplyEquationFailure (EquationT io) ()
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
        ExceptT ApplyEquationFailure (EquationT io) ()
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
        (Term -> ExceptT ApplyEquationFailure (EquationT io) ()) ->
        ExceptT ApplyEquationFailure (EquationT io) ()
    verifyVar subst (variableName, sortName) check =
        maybe
            ( lift . throw . InternalError . Text.pack $
                "Variable not found: " <> show (variableName, sortName)
            )
            check
            $ Map.lookup Variable{variableSort = SortApp sortName [], variableName} subst

--------------------------------------------------------------------

{- Simplification for boolean predicates

    This is used during rewriting to simplify side conditions of rules
    (to decide whether or not a rule can apply, not to retain the
    ensured conditions).

    If and as soon as this function is used inside equation
    application, it needs to run within the same 'EquationT' context
    so we can detect simplification loops and avoid monad nesting.
-}
simplifyConstraint ::
    MonadLoggerIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    Predicate ->
    io (Either EquationFailure Predicate, [EquationTrace Term], SimplifierCache)
simplifyConstraint doTracing def mbApi mbSMT cache (Predicate p) =
    runEquationT doTracing def mbApi mbSMT cache $ (coerce <$>) . simplifyConstraint' True $ p

simplifyConstraints ::
    MonadLoggerIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    [Predicate] ->
    io (Either EquationFailure [Predicate], [EquationTrace Term], SimplifierCache)
simplifyConstraints doTracing def mbApi mbSMT cache ps =
    runEquationT doTracing def mbApi mbSMT cache $
        concatMap splitAndBools
            <$> mapM ((coerce <$>) . simplifyConstraint' True . coerce) ps

-- version for internal nested evaluation
simplifyConstraint' :: MonadLoggerIO io => Bool -> Term -> EquationT io Term
-- Evaluates terms of boolean sort (coming from predicates of the form
-- 'true \equals P' using simplifyBool if they are concrete, or using
-- evaluateTerm.
simplifyConstraint' recurseIntoEvalBool = \case
    t@(Term TermAttributes{canBeEvaluated} _)
        | isConcrete t && canBeEvaluated -> do
            mbApi <- (.llvmApi) <$> getConfig
            case mbApi of
                Just api ->
                    LLVM.simplifyBool api t >>= \case
                        Left e ->
                            throw $ UndefinedTerm t e
                        Right res ->
                            pure $
                                if res
                                    then TrueBool
                                    else FalseBool
                Nothing -> if recurseIntoEvalBool then evalBool t else pure t
        | otherwise ->
            if recurseIntoEvalBool then evalBool t else pure t
  where
    evalBool :: MonadLoggerIO io => Term -> EquationT io Term
    evalBool t = do
        prior <- getState -- save prior state so we can revert
        eqState $ put prior{termStack = mempty, changed = False}
        result <- iterateEquations BottomUp PreferFunctions t
        eqState $ put prior
        pure result
