{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Parsing attributes from @ParsedAttributes@ to different internal
types. The required attribute names and parsers for the expected
values are hard-wired.
-}

module Kore.Definition.Attributes (
    HasAttributes(..),
) where

import Data.Kind
import Data.Word
import Data.Text (Text)
import Data.Text qualified as Text

import Kore.Syntax.ParsedKore.Base
import Kore.Definition.Base

-- | A class describing all attributes we want to extract from parsed
-- entities
class HasAttributes tipe where
    type Attributes tipe :: Type

    extract :: ParsedAttributes -> Attributes tipe

instance HasAttributes ParsedDefinition where
    type Attributes ParsedDefinition = DefinitionAttributes

    extract _ = DefinitionAttributes

instance HasAttributes ParsedModule where
    type Attributes ParsedModule = ModuleAttributes

    extract _ = ModuleAttributes

instance HasAttributes ParsedAxiom where
    type Attributes ParsedAxiom = AxiomAttributes

    extract attribs =
        AxiomAttributes
             (extractAttribute axiomLocation attribs)
             (extractAttributeOrDefault 50 axiomPriority attribs)
             (extractAttributeOrDefault "" axiomLabel attribs)
      where
        axiomLocation = "name of location attribute"
        axiomPriority = "priority"
        axiomLabel = "label"


----------------------------------------

extractAttribute :: ReadT a => Text -> ParsedAttributes -> a
extractAttribute name attribs = undefined

extractAttributeOrDefault :: ReadT a => a -> Text -> ParsedAttributes -> a
extractAttributeOrDefault def name attribs = undefined

class ReadT a where
    parseValue :: Text -> a
    default parseValue :: Read a => Text -> a
    parseValue = read . Text.unpack

instance ReadT Word8
instance ReadT Location where
    parseValue = undefined
instance ReadT Label
