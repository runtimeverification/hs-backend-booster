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

import Control.Monad.Trans.State
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Read (readMaybe)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.SMT.Base as SMT
import Booster.SMT.LowLevelCodec as SMT

data TranslationState =
    TranslationState
        { mappings :: Map Term SmtId
        , counter :: !Int
        }

initTranslator :: TranslationState
initTranslator =
    TranslationState{mappings = mempty, counter = 1}

newtype Translator a = Translator (State TranslationState a)
    deriving newtype (Functor, Applicative, Monad)

asSMTVar :: Term -> Translator SExpr
asSMTVar t = Translator $ do
    st <- get
    case Map.lookup t st.mappings of
        Just v -> pure $ Atom v
        Nothing -> do
            let new = SmtId . BS.pack $ "SMT-" <> show st.counter
            put st
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
            translateTerm t' -- ???
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
                else list!!(n-1)
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

          -- free variables (not created by abstraction during
          -- translation) are all-quantified on the outside
          let freeVars = Set.toList (getAttributes equation.lhs).variables
              -- TODO is the LHS enough? The RHS may have existentials.
              mkSExpPair :: Variable -> SExpr
              mkSExpPair v =
                  List [Atom $ SmtId v.variableName, SMT.sortExpr $ smtSort v.variableSort]
          pure $ Assert $ List [Atom "forall", List $ map mkSExpPair freeVars, lemmaRaw]
          -- FIXME also dump all declare-const from the TranslationState mappings
          -- and reset the mappings afterwards

-- collect and render all declarations from a definition
smtDeclarations :: KoreDefinition -> Translator [DeclareCommand]
smtDeclarations def = undefined
    -- declare all sorts except Int and Bool
    -- declare all functions that have smt-lib
    -- kore-rpc also declares all constructors, with no-junk axioms. WHY?
    -- declare all SMT lemmas (see functions above) as assertions

smtSort :: Sort -> SmtSort
smtSort (SortApp sortName args)
    | null args = SimpleSmtSort $ SmtId sortName
    | otherwise = SmtSort (SmtId sortName) $ map smtSort args
smtSort (SortVar varName) =
    SimpleSmtSort $ SmtId varName -- of course not previously declared...???
