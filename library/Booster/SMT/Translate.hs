{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Translate (
    TranslationState (..),
    Translator (..),
    initTranslator,
    translateTerm,
    smtDeclarations,
) where

import Control.Monad.Trans.State
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Read (readMaybe)

import Booster.Definition.Attributes.Base (SymbolAttributes (..), SMTType (..))
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.SMT.Base

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



-- render an SMT assertion from an SMT lemma (which exist for both kinds of equations)
simplificationLemma :: RewriteRule "Simplification" -> Translator DeclareCommand
simplificationLemma = undefined
functionLemma :: RewriteRule "Function" -> Translator DeclareCommand
functionLemma = undefined

-- collect and render all declarations from a definition
smtDeclarations :: KoreDefinition -> Translator [DeclareCommand]
smtDeclarations def = undefined
    -- declare all sorts except Int and Bool
    -- declare all functions that have smt-lib
    -- declare all SMT lemmas (see functions above) as assertions
