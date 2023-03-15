{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Syntax.Json.Internalise (
    internalisePattern,
    internaliseTermOrPredicate,
    internaliseTerm,
    internalisePredicate,
    PatternError (..),
    internaliseSort,
    SortError (..),
    renderSortError,
    ----------------
    textToBS,
) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Except
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable ()
import Data.List (foldl1', nub)
import Data.List.NonEmpty as NE (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding (decodeLatin1)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base (KoreDefinition (..))
import Booster.Pattern.Base qualified as Internal
import Booster.Pattern.Util (sortOfTerm)
import Booster.Syntax.Json.Base qualified as Syntax
import Booster.Syntax.Json.Externalise (externaliseSort)

internalisePattern ::
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Pattern
internalisePattern sortVars definition p = do
    (terms, predicates) <- partitionM isTermM $ explodeAnd p

    when (null terms) $ throwE $ NoTermFound p

    -- construct an AndTerm from all terms (checking sort consistency)
    term <- andTerm p =<< mapM (internaliseTerm sortVars definition) terms
    -- internalise all predicates
    constraints <- mapM (internalisePredicate sortVars definition) predicates
    pure Internal.Pattern{term, constraints}
  where
    andTerm :: Syntax.KorePattern -> [Internal.Term] -> Except PatternError Internal.Term
    andTerm _ [] = error "BUG: andTerm called with empty term list"
    andTerm pat ts = do
        let sortList = nub $ map sortOfTerm ts
            resultTerm = foldl1' Internal.AndTerm ts
        -- check resulting terms for consistency and sorts
        -- TODO needs to consider sub-sorts instead (set must be
        -- consistent) if this code becomes order-sorted
        unless (length sortList == 1) $
            throwE $
                PatternSortError pat (IncompatibleSorts $ map externaliseSort sortList)
        pure resultTerm

internaliseTermOrPredicate ::
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except [PatternError] Internal.TermOrPredicate
internaliseTermOrPredicate sortVars definition syntaxPatt =
    Internal.APredicate <$> (withExcept (: []) $ internalisePredicate sortVars definition syntaxPatt)
        <|> Internal.TermAndPredicate
            <$> (withExcept (: []) $ internalisePattern sortVars definition syntaxPatt)

lookupInternalSort ::
    Maybe [Syntax.Id] ->
    Map Internal.SortName (SortAttributes, Set Internal.SortName) ->
    Syntax.KorePattern ->
    Syntax.Sort ->
    Except PatternError Internal.Sort
lookupInternalSort sortVars sorts pat =
    let knownVarSet = maybe Set.empty (Set.fromList . map Syntax.getId) sortVars
     in mapExcept (first $ PatternSortError pat) . internaliseSort knownVarSet sorts

-- Throws errors when a predicate is encountered. The 'And' case
-- should be analysed before, this function produces an 'AndTerm'.
internaliseTerm ::
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Term
internaliseTerm sortVars definition@KoreDefinition{sorts, symbols} pat =
    case pat of
        Syntax.KJEVar{name, sort} -> do
            variableSort <- lookupInternalSort' sort
            let variableName = textToBS name.getId
            pure $ Internal.Var Internal.Variable{variableSort, variableName}
        Syntax.KJSVar{name, sort} -> do
            variableSort <- lookupInternalSort' sort
            let variableName = textToBS name.getId
            pure $ Internal.Var Internal.Variable{variableSort, variableName}
        Syntax.KJApp{name, sorts = [from, to], args = [arg]}
            | Just Internal.injectionSymbol == Map.lookup (textToBS name.getId) symbols -> do
                from' <- lookupInternalSort' from
                to' <- lookupInternalSort' to
                arg' <- recursion arg
                pure $ Internal.Injection from' to' arg'
        symPatt@Syntax.KJApp{name, sorts = appSorts, args} -> do
            symbol <-
                maybe (throwE $ UnknownSymbol name symPatt) pure $
                    Map.lookup (textToBS name.getId) symbols
            -- Internalise sort variable instantiation (appSorts)
            -- Length must match sort variables in symbol declaration.
            unless (length appSorts == length symbol.sortVars) $
                throwE $
                    PatternSortError pat $
                        GeneralError
                            "wrong sort argument count for symbol"
            Internal.SymbolApplication symbol
                <$> mapM lookupInternalSort' appSorts
                <*> mapM recursion args
        Syntax.KJString{value} ->
            pure $ Internal.DomainValue (Internal.SortApp "SortString" []) $ textToBS value
        Syntax.KJTop{} -> predicate
        Syntax.KJBottom{} -> predicate
        Syntax.KJNot{} -> predicate
        Syntax.KJAnd{first = arg1, second = arg2} -> do
            -- analysed beforehand, expecting this to operate on terms
            a <- recursion arg1
            b <- recursion arg2
            -- TODO check that both a and b are of sort "resultSort"
            -- Which is a unification problem if this involves variables.
            pure $ Internal.AndTerm a b
        Syntax.KJOr{} -> predicate
        Syntax.KJImplies{} -> predicate
        Syntax.KJIff{} -> predicate
        Syntax.KJForall{} -> predicate
        Syntax.KJExists{} -> predicate
        Syntax.KJMu{} -> predicate
        Syntax.KJNu{} -> predicate
        Syntax.KJCeil{} -> predicate
        Syntax.KJFloor{} -> predicate
        Syntax.KJEquals{} -> predicate
        Syntax.KJIn{} -> predicate
        Syntax.KJNext{} -> predicate
        Syntax.KJRewrites{} -> predicate
        Syntax.KJDV{sort, value} ->
            Internal.DomainValue
                <$> lookupInternalSort' sort
                <*> pure (textToBS value)
        Syntax.KJMultiOr{} -> predicate
        Syntax.KJLeftAssoc{symbol, sorts = argSorts, argss} ->
            recursion $ foldl1 (mkF symbol argSorts) argss
        Syntax.KJRightAssoc{symbol, sorts = argSorts, argss} ->
            recursion $ foldr1 (mkF symbol argSorts) argss
  where
    predicate = throwE $ TermExpected pat

    lookupInternalSort' = lookupInternalSort sortVars sorts pat

    recursion = internaliseTerm sortVars definition

-- Throws errors when a term is encountered. The 'And' case
-- is analysed before, this function produces an 'AndPredicate'.
internalisePredicate ::
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Predicate
internalisePredicate sortVars definition@KoreDefinition{sorts} pat = case pat of
    Syntax.KJEVar{} -> term
    Syntax.KJSVar{} -> term
    Syntax.KJApp{} -> term
    Syntax.KJString{} -> term
    Syntax.KJTop{} -> do
        pure Internal.Top
    Syntax.KJBottom{} -> do
        pure Internal.Bottom
    Syntax.KJNot{arg} -> do
        Internal.Not <$> recursion arg
    Syntax.KJAnd{first = arg1, second = arg2} -> do
        -- consistency should have been checked beforehand,
        -- building an AndPredicate
        Internal.AndPredicate
            <$> recursion arg1
            <*> recursion arg2
    Syntax.KJOr{first = arg1, second = arg2} ->
        Internal.Or
            <$> recursion arg1
            <*> recursion arg2
    Syntax.KJImplies{first = arg1, second = arg2} ->
        Internal.Implies
            <$> recursion arg1
            <*> recursion arg2
    Syntax.KJIff{first = arg1, second = arg2} ->
        Internal.Iff
            <$> recursion arg1
            <*> recursion arg2
    Syntax.KJForall{var, arg} ->
        Internal.Forall (textToBS var.getId) <$> recursion arg
    Syntax.KJExists{var, arg} ->
        Internal.Exists (textToBS var.getId) <$> recursion arg
    Syntax.KJMu{} -> notSupported
    Syntax.KJNu{} -> notSupported
    Syntax.KJCeil{arg} ->
        Internal.Ceil <$> internaliseTerm sortVars definition arg
    Syntax.KJFloor{} -> notSupported
    Syntax.KJEquals{argSort, first = arg1, second = arg2} -> do
        -- distinguish term and predicate equality
        is1Term <- isTermM arg1
        is2Term <- isTermM arg2
        case (is1Term, is2Term) of
            (True, True) -> do
                a <- internaliseTerm sortVars definition arg1
                b <- internaliseTerm sortVars definition arg2
                argS <- lookupInternalSort' argSort
                -- check that argS and sorts of a and b "agree"
                ensureEqualSorts (sortOfTerm a) argS
                ensureEqualSorts (sortOfTerm b) argS
                pure $ Internal.EqualsTerm a b
            (False, False) ->
                Internal.EqualsPredicate
                    <$> recursion arg1
                    <*> recursion arg2
            _other ->
                throwE $ InconsistentPattern pat
    Syntax.KJIn{sort, first = arg1, second = arg2} -> do
        a <- internaliseTerm sortVars definition arg1
        b <- internaliseTerm sortVars definition arg2
        s <- lookupInternalSort' sort
        -- check that `sort` and sorts of a and b agree
        ensureEqualSorts (sortOfTerm a) s
        ensureEqualSorts (sortOfTerm b) s
        pure $ Internal.In a b
    Syntax.KJNext{} -> notSupported
    Syntax.KJRewrites{} -> notSupported -- should only occur in claims!
    Syntax.KJDV{} -> term
    Syntax.KJMultiOr{assoc, sort, argss} ->
        recursion $ withAssoc assoc (Syntax.KJOr sort) argss
    Syntax.KJLeftAssoc{} -> term
    Syntax.KJRightAssoc{} -> term
  where
    term = throwE $ PredicateExpected pat
    notSupported = throwE $ NotSupported pat

    recursion = internalisePredicate sortVars definition

    lookupInternalSort' = lookupInternalSort sortVars sorts pat

    -- Recursively check that two (internal) sorts are the same.
    -- Sort variables must be eliminated (instantiated) before checking.
    ensureEqualSorts :: Internal.Sort -> Internal.Sort -> Except PatternError ()
    ensureEqualSorts s s' = mapExcept (first $ PatternSortError pat) $ go s s'
      where
        go :: Internal.Sort -> Internal.Sort -> Except SortError ()
        go s1 s2 =
            case (s1, s2) of
                (Internal.SortVar n, _) ->
                    throwE $ GeneralError ("ensureSortsAgree found variable " <> decodeLatin1 n)
                (_, Internal.SortVar n) ->
                    throwE $ GeneralError ("ensureSortsAgree found variable " <> decodeLatin1 n)
                (Internal.SortApp name1 args1, Internal.SortApp name2 args2) -> do
                    unless (name1 == name2) $
                        throwE (IncompatibleSorts (map externaliseSort [s1, s2]))
                    zipWithM_ go args1 args2

----------------------------------------

-- converts MultiOr to a chain at syntax level
withAssoc :: Syntax.LeftRight -> (a -> a -> a) -> NonEmpty a -> a
withAssoc Syntax.Left = foldl1
withAssoc Syntax.Right = foldr1

-- for use with withAssoc
mkF ::
    Syntax.Id ->
    [Syntax.Sort] ->
    Syntax.KorePattern ->
    Syntax.KorePattern ->
    Syntax.KorePattern
mkF symbol argSorts a b = Syntax.KJApp symbol argSorts [a, b]

-- primitive solution ignoring text encoding
textToBS :: Text -> ByteString
textToBS = BS.pack . Text.unpack

----------------------------------------

{- | Given a set of sort variable names and a sort attribute map, checks
   a given syntactic @Sort@ and converts to an internal Sort
-}
internaliseSort ::
    Set Text ->
    Map Internal.SortName (SortAttributes, Set Internal.SortName) ->
    Syntax.Sort ->
    Except SortError Internal.Sort
internaliseSort knownVars sortMap = check'
  where
    check' :: Syntax.Sort -> Except SortError Internal.Sort
    check' var@Syntax.SortVar{name = Syntax.Id n} = do
        unless (n `Set.member` knownVars) $
            throwE (UnknownSort var)
        pure $ Internal.SortVar $ textToBS n
    check' app@Syntax.SortApp{name = Syntax.Id n, args} =
        do
            let name = textToBS n
            maybe
                (throwE $ UnknownSort app)
                ( \(SortAttributes{argCount}, _) ->
                    unless (length args == argCount) $
                        throwE (WrongSortArgCount app argCount)
                )
                (Map.lookup name sortMap)
            internalArgs <- mapM check' args
            pure $ Internal.SortApp name internalArgs

isTermM :: Syntax.KorePattern -> Except PatternError Bool
isTermM pat = case pat of
    Syntax.KJEVar{} -> pure True
    Syntax.KJSVar{} -> pure True
    Syntax.KJApp{} -> pure True
    Syntax.KJString{} -> pure True
    Syntax.KJTop{} -> pure False
    Syntax.KJBottom{} -> pure False
    Syntax.KJNot{} -> pure False
    Syntax.KJAnd{first = arg1, second = arg2} -> do
        a1Term <- isTermM arg1
        a2Term <- isTermM arg2
        when (a1Term /= a2Term) $ throwE (InconsistentPattern pat)
        pure a1Term
    Syntax.KJOr{} -> pure False
    Syntax.KJImplies{} -> pure False
    Syntax.KJIff{} -> pure False
    Syntax.KJForall{} -> pure False
    Syntax.KJExists{} -> pure False
    Syntax.KJMu{} -> notSupported
    Syntax.KJNu{} -> notSupported
    Syntax.KJCeil{} -> pure False
    Syntax.KJFloor{} -> pure False
    Syntax.KJEquals{} -> pure False
    Syntax.KJIn{} -> pure False
    Syntax.KJNext{} -> notSupported
    Syntax.KJRewrites{} -> notSupported -- should only occur in claims
    Syntax.KJDV{} -> pure True
    Syntax.KJMultiOr{} -> pure False
    Syntax.KJLeftAssoc{} -> pure True
    Syntax.KJRightAssoc{} -> pure True
  where
    notSupported = throwE $ NotSupported pat

{- | unpacks the top level of 'And' nodes of a 'KorePattern', returning
   the arguments as a list. The top-level sorts of the 'And' nodes are
   ignored, no checks are performed.
-}
explodeAnd :: Syntax.KorePattern -> [Syntax.KorePattern]
explodeAnd Syntax.KJAnd{first = arg1, second = arg2} =
    explodeAnd arg1 <> explodeAnd arg2
explodeAnd other = [other]

----------------------------------------

data PatternError
    = NotSupported Syntax.KorePattern
    | NoTermFound Syntax.KorePattern
    | PatternSortError Syntax.KorePattern SortError
    | InconsistentPattern Syntax.KorePattern
    | TermExpected Syntax.KorePattern
    | PredicateExpected Syntax.KorePattern
    | UnknownSymbol Syntax.Id Syntax.KorePattern
    deriving stock (Eq, Show)

{- | ToJson instance (user-facing):

Renders an error string as 'error' and providing the pattern or Id in
a 'context' list.

The JSON-RPC server's error responses contain both of these fields in
a 'data' object.

If the error is a sort error, the message will contain its information
while the context provides the pattern where the error occurred.
-}
instance ToJSON PatternError where
    toJSON = \case
        NotSupported p ->
            wrap "Pattern not supported" p
        NoTermFound p ->
            wrap "Pattern must contain at least one term" p
        PatternSortError p sortErr ->
            wrap (renderSortError sortErr) p
        InconsistentPattern p ->
            wrap "Inconsistent pattern" p
        TermExpected p ->
            wrap "Expected a term but found a predicate" p
        PredicateExpected p ->
            wrap "Expected a predicate but found a term" p
        UnknownSymbol sym p ->
            wrap ("Unknown symbol " <> Syntax.getId sym) p
      where
        wrap :: Text -> Syntax.KorePattern -> Value
        wrap msg p = object ["error" .= msg, "context" .= toJSON [p]]

data SortError
    = UnknownSort Syntax.Sort
    | WrongSortArgCount Syntax.Sort Int
    | IncompatibleSorts [Syntax.Sort]
    | GeneralError Text
    deriving stock (Eq, Show)

renderSortError :: SortError -> Text
renderSortError = \case
    UnknownSort sort ->
        "Unknown " <> render sort
    WrongSortArgCount sort expected ->
        "Wrong argument count: expected "
            <> pack (show expected)
            <> " in "
            <> render sort
    IncompatibleSorts sorts ->
        "Incompatible sorts: " <> intercalate ", " (map render sorts)
    GeneralError msg ->
        msg
  where
    render = \case
        Syntax.SortVar (Syntax.Id n) ->
            "sort variable " <> n
        Syntax.SortApp (Syntax.Id n) args ->
            "sort " <> n <> "(" <> intercalate ", " (map render args) <> ")"
