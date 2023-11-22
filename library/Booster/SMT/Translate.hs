{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Translate (
    TranslationState (..),
    Translator (..),
    equationToSmtLemma,
    initTranslator,
    smtDeclarations,
    translateTerm,
) where

import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Prettyprinter qualified as Pretty
import Text.Read (readMaybe)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Prettyprinter qualified as Pretty
import Booster.SMT.Base as SMT
import Booster.SMT.LowLevelCodec as SMT

data TranslationState = TranslationState
    { mappings :: Map Term SmtId
    , counter :: !Int
    }

initTranslator :: TranslationState
initTranslator =
    TranslationState{mappings = mempty, counter = 1}

newtype Translator a = Translator (State TranslationState a)
    deriving newtype (Functor, Applicative, Monad)

runTranslator :: Translator a -> (a, TranslationState)
runTranslator (Translator action) = runState action initTranslator

asSMTVar :: Term -> Translator SExpr
asSMTVar t = Translator $ do
    st <- get
    case Map.lookup t st.mappings of
        Just v -> pure $ Atom v
        Nothing -> do
            let new = SmtId . BS.pack $ "SMT-" <> show st.counter
            put
                st
                    { mappings = Map.insert t new st.mappings
                    , counter = st.counter + 1
                    }
            pure $ Atom new

translateTerm :: Term -> Translator SExpr
translateTerm t =
    case t of
        AndTerm t1 t2 -> error "what now"
        SymbolApplication sym _sorts args ->
            case sym.attributes.smt of
                Nothing -> asSMTVar t
                Just (SMTLib name) -> do
                    smtArgs <- mapM translateTerm args
                    pure . List $ Atom (SmtId name) : smtArgs
                Just (SMTHook hook@Atom{}) -> do
                    smtArgs <- mapM translateTerm args
                    pure . List $ hook : smtArgs
                Just (SMTHook sexpr) -> do
                    smtArgs <- mapM translateTerm args
                    pure $ fillPlaceholders sexpr smtArgs
        DomainValue sort value
            | SortBool <- sort ->
                pure $ Atom (SmtId value)
            | SortInt <- sort ->
                pure $ Atom (SmtId value)
            | otherwise ->
                asSMTVar t
        Var{} ->
            asSMTVar t
        Injection _s1 _s2 t' ->
            translateTerm t'
        _other ->
            asSMTVar t

-- Atoms of the shape "#<num>" where <num> is a small positive
-- integer are replaced with the element at index <num>.
fillPlaceholders :: SExpr -> [SExpr] -> SExpr
fillPlaceholders target list = go target
  where
    go :: SExpr -> SExpr
    go (Atom symb) = fillAtom symb
    go (List sexprs) = List (map go sexprs)

    maxArg = length list

    fillAtom :: SmtId -> SExpr
    fillAtom name@(SmtId bs)
        | '#' == BS.head bs
        , BS.length bs > 1
        , Just n <- readMaybe @Int (BS.unpack $ BS.tail bs) =
            if n > maxArg
                then error $ "Hook argument index out of bounds: " <> show target
                else list !! (n - 1)
        | otherwise = Atom name

-- render an SMT assertion from an SMT lemma (which exist for both
-- kinds of equations,"Function" and "Simplification")
equationToSmtLemma :: RewriteRule a -> Translator (Maybe DeclareCommand)
equationToSmtLemma equation
    | not (coerce equation.attributes.smtLemma) = pure Nothing
    | otherwise = fmap Just $ do
        smtLHS <- translateTerm equation.lhs
        smtRHS <- translateTerm equation.rhs
        let equationRaw = SMT.eq smtLHS smtRHS
        -- if requires empty: just (= (lhs) (rhs))
        -- if requires not empty: (=> (requires) (= (lhs) (rhs)))
        lemmaRaw <-
            if Set.null equation.requires
                then pure equationRaw
                else do
                    smtPremise <-
                        foldl1 SMT.and
                            <$> mapM (translateTerm . \(Predicate t) -> t) (Set.toList equation.requires)
                    pure $ SMT.implies smtPremise equationRaw
        -- NB ensures has no SMT implications (single shot sat-check)

        finalMapping <- Translator $ gets (.mappings)
        -- for detailed error messages:
        let prettyMappings m =
                Pretty.renderDefault $
                    Pretty.vsep
                        [ Pretty.pretty (show v) <> " <== " <> Pretty.pretty t
                        | (t, v) <- Map.toList m
                        ]
            lemmaId =
                Pretty.renderDefault . head $
                    catMaybes
                        [ fmap Pretty.pretty equation.attributes.ruleLabel
                        , fmap Pretty.pretty equation.attributes.location
                        , Just "Unknown location"
                        ]
        -- free variables (not created by abstraction during
        -- translation) are all-quantified on the outside
        let freeVars = Set.toList (getAttributes equation.lhs).variables
            -- TODO is the LHS enough? The RHS may have existentials.
            mkSExpPair :: Variable -> SExpr
            mkSExpPair v
                | Just smtV <- Map.lookup (Var v) finalMapping =
                    List [Atom smtV, SMT.sortExpr $ smtSort v.variableSort]
                | otherwise =
                    error $
                        unlines
                            [ "Free variable " <> show v.variableName <> " not found in "
                            , prettyMappings finalMapping
                            ]
        -- An SMT lemma should not contain any uninterpreted
        -- functions. If anything was variable-abstracted apart from
        -- the free variables in the term, this is an error.
        let surplusMappings = foldr (Map.delete . Var) finalMapping freeVars
        unless (Map.null surplusMappings) $ do
            error $
                unlines
                    [ "Surplus mappings found for lemma " <> lemmaId
                    , prettyMappings surplusMappings
                    ]
        -- reset state but keep variable counter
        Translator . modify $ \s -> s{mappings = Map.empty}
        pure $ Assert $ List [Atom "forall", List $ map mkSExpPair freeVars, lemmaRaw]

-- collect and render all declarations from a definition
smtDeclarations :: KoreDefinition -> [DeclareCommand]
smtDeclarations def
    | not (Map.null finalState.mappings) =
        error $ "Unexpected final state " <> show (finalState.mappings, finalState.counter)
    | otherwise =
        concat [sortDecls, funDecls, lemmas]
  where
    -- declare all sorts except Int and Bool
    sortDecls =
        [ DeclareSort (smtName name) attributes.argCount
        | (name, (attributes, _)) <- Map.assocs def.sorts
        , name /= "SortInt"
        , name /= "SortBool"
        ]
    -- declare all functions that have smt-lib
    funDecls =
        mapMaybe declareFunc $ Map.elems def.symbols

    -- declare all SMT lemmas as assertions
    allRules :: Map k (Map k' [v]) -> [v]
    allRules = concat . concatMap Map.elems . Map.elems
    extractLemmas = fmap catMaybes . mapM equationToSmtLemma . allRules

    (lemmas, finalState) =
        runTranslator $
            (<>) <$> extractLemmas def.functionEquations <*> extractLemmas def.simplifications

    -- kore-rpc also declares all constructors, with no-junk axioms. WHY?

    declareFunc :: Symbol -> Maybe DeclareCommand
    declareFunc sym
        | Just (SMTLib name) <- sym.attributes.smt =
            Just $ DeclareFunc (smtName name) (map smtSort sym.argSorts) (smtSort sym.resultSort)
        | otherwise = Nothing

smtName :: BS.ByteString -> SmtId
smtName = SmtId -- keep it simple

smtSort :: Sort -> SmtSort
smtSort SortInt = SimpleSmtSort "Int"
smtSort SortBool = SimpleSmtSort "Bool"
smtSort (SortApp sortName args)
    | null args = SimpleSmtSort $ smtName sortName
    | otherwise = SmtSort (smtName sortName) $ map smtSort args
smtSort (SortVar varName) =
    error $ "Sort variable " <> show varName <> " not supported for SMT"

-- SimpleSmtSort $ smtName varName -- of course not previously declared...???
