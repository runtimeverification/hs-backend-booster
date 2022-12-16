{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Pattern.Base (
    -- export everything, modules above can re-export only type names
    module Kore.Pattern.Base,
) where

import Control.DeepSeq (NFData (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kore.Definition.Attributes.Base (SymbolAttributes)

type VarName = Text
type SymbolName = Text
type SortName = Text

{- | A term has a particular 'Sort', which is part of a definition.
  Sorts can be subsorts of others (not represented in the definition).
-}
data Sort
    = -- | sort constructor, potentially with arguments
      SortApp SortName [Sort]
    | -- | sort variable (symbolic)
      SortVar VarName
    deriving stock (Eq, Ord, Show)

pattern SortBool :: Sort
pattern SortBool = SortApp "SortBool" []

-- | A variable for symbolic execution or for terms in a rule.
data Variable = Variable
    { variableSort :: Sort
    , variableName :: VarName
    }
    deriving (Eq, Ord, Show)

data Symbol = Symbol
    { name :: SymbolName
    , sortVars :: [VarName]
    , argSorts :: [Sort]
    , resultSort :: Sort
    , attributes :: SymbolAttributes
    }
    deriving stock (Eq, Ord, Show)

{- | A term consists of an AST of constructors and function calls, as
   well as domain values (tokens and built-in types) and (element)
   variables.
   This is anything that can be part of a K configuration.

   Deliberately kept simple in this codebase (leaving out built-in
   types and containers).
-}
data Term
    = AndTerm Term Term -- used in #as patterns
    | SymbolApplication Symbol [Sort] [Term]
    | DomainValue Sort Text
    | Var Variable
    deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Term

pattern AndBool :: [Term] -> Term
pattern AndBool ts <- SymbolApplication (Symbol "Lbl'Unds'andBool'Unds'" _ _ _ _) _ ts

{- | A predicate describes constraints on terms. It will always evaluate
   to 'Top' or 'Bottom'. Notice that 'Predicate's don't have a sort.
-}
data Predicate
    = AndPredicate Predicate Predicate
    | Bottom
    | Ceil Term
    | EqualsTerm Term Term
    | EqualsPredicate Predicate Predicate -- I remember running into this one a few times, but I'm not sure if it was an integration test or a unit test
    | Exists VarName Predicate
    | Forall VarName Predicate -- do we need forall?
    | Iff Predicate Predicate
    | Implies Predicate Predicate
    | In Term Term
    | Not Predicate
    | Or Predicate Predicate
    | Top
    deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Predicate

-- | A term (configuration) constrained by a number of predicates.
data Pattern = Pattern
    { term :: Term
    , constraints :: [Predicate]
    }
    deriving stock (Eq, Ord, Show)

data TermOrPredicate -- = Either Predicate Pattern
    = APredicate Predicate
    | TermAndPredicate Pattern
    deriving stock (Eq, Ord, Show)

{- | Index data allowing for a quick lookup of potential axioms.

A @Term@ is indexed by inspecting the top term component of the
head of the K cell. Only constructor and (other) symbol
applications are indexed, all other terms have index @Anything@.

In particular, function applications are treated as opaque, like
variables.

Also, non-free constructors won't get any index, any rules headed by
those can be ignored.

Rather than making the term indexing function partial, we introduce a
unique bottom element @None@ to the index type (to make it a lattice).
This can then handle @AndTerm@ by indexing both arguments and
combining them.

NB we should not derive an 'Ord' instance since it would not reflect
the fact that different symbols (and likewise different constructors)
are incompatible.
-}
data TermIndex
    = None -- bottom element
    | TopSymbol SymbolName
    | Anything -- top element
    -- should we have  | Value Sort ?? (see Term type)
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- | Combines two indexes (an "infimum" function on the index lattice)
combine :: TermIndex -> TermIndex -> TermIndex
combine None _ = None
combine _ None = None
combine x Anything = x
combine Anything x = x
combine s@(TopSymbol s1) (TopSymbol s2)
    | s1 == s2 = s
--     | otherwise = None -- redundant
combine _ _ = None -- incompatible indexes
