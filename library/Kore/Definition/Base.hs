{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

@Definition@ models a K definition (a main module with its transitive
imports) that has been verified and is optimised for the needs of the
simple rewriter matching pattern terms to rule LHS terms.

Axioms are stored in a lookup map according to the _index_ of their LHS,
and in groups of equal priority (descending order).

Symbols (and constructors) are stored in a lookup table by their name.
-}

module Kore.Definition.Base (
    module Kore.Definition.Base,
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word

import Kore.Pattern.Base

{- | Index data allowing for a quick lookup of potential axioms.

A @Term@ is indexed by inspecting the top term component of the
head of the K cell. Only constructor and (other) symbol
applications are indexed, all other terms have index @Anything@.

In particular, function applications are treated as opaque, like
variables.
-}
data TermIndex
    = Symbol SymbolName
    | Value Sort  -- should we have this ???
    | Anything
    deriving (Eq, Ord, Show)

{- | A Kore definition is constructed from a main module with its
   transitive imports.

All sentences are gathered together and their data stored in different
fields, depending on the sentence type.

A consistent import hierarchy and scoping is not guaranteed within the
data type, but rather by its construction from a @ParsedDefinition@.
-}
data KoreDefinition
    = KoreDefinition
      { attributes :: DefinitionAttributes
      , modules :: Map Text ModuleAttributes
      , sorts :: Map SortName SortAttributes  -- TODO store a lattice of subsorts?
      , symbols :: Map SymbolName SymbolAttributes -- constructors and functions
      -- , aliases
      , axioms :: Map TermIndex [Axiom]
      -- , claims
      }
    deriving (Eq, Show)

data Axiom
    = Axiom
      { lhs :: Pattern
      , rhs :: Pattern
      , attributes :: AxiomAttributes
      }
    deriving (Eq, Show)

-- attributes for different components
-- TODO move to its own module? Not enough at the moment.
data DefinitionAttributes
    = DefinitionAttributes
      { -- none needed
      }
    deriving (Eq, Show)

data ModuleAttributes
    = ModuleAttributes
      { -- none needed
      }
    deriving (Eq, Show)

data AxiomAttributes
    = AxiomAttributes
      { location :: Location
      , priority :: Word8 -- priorities are <= 200
      , label :: Label
      }
    deriving (Eq, Show)

type Label = Text

data Location
    = Location
      { file :: FilePath
      , positon :: Position
      }
    deriving (Eq, Ord, Show)

data Position
    = Position
      { line :: Int
      , column :: Int
      }
    deriving (Eq, Ord, Show)

data SymbolAttributes
    = SymbolAttributes
      { isFunction :: Bool
      , isTotal :: Bool
      , isFree :: Bool  -- ??? replace by all things implying "non-free"
      , isConstructor :: Bool
      }
    deriving (Eq, Show)

data SortAttributes
    = SortAttributes
      { -- none needed
      }
    deriving (Eq, Show)
