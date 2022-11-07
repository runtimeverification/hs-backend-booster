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
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Text (Text)
import Data.Text qualified as Text

import Kore.Syntax.ParsedKore.Base
import Kore.Definition.Attributes.Base

-- TODO maybe write a proper applicative parsing framework for these attributes (later)

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
             location
             (attribs .:? "priority" $ 50)
             undefined -- (attribs .:? "label" $ Nothing)
             (attribs .! "simplification")
      where
        location = Location (attribs .: sourceName) (attribs .: locationName)
        sourceName = "org'Stop'kframework'Stop'attributes'Stop'Source"
        locationName = "org'Stop'kframework'Stop'attributes'Stop'Location"

instance HasAttributes ParsedSymbol where
    type Attributes ParsedSymbol = SymbolAttributes

    extract attribs =
        SymbolAttributes
        { isFunction = extractFlag "function" attribs
        , isTotal = extractFlag "functional" attribs || extractFlag "total"  attribs
        , isConstructor = extractFlag "constructor" attribs
        }

----------------------------------------

extractAttribute :: ReadT a => Text -> ParsedAttributes -> a
extractAttribute name
    = extractAttributeOrDefault (error $ show name <> " not found in attributes") name

(.:) :: ReadT a => ParsedAttributes -> Text -> a
(.:) = flip extractAttribute

extractAttributeOrDefault :: ReadT a => a -> Text -> ParsedAttributes -> a
extractAttributeOrDefault def name attribs
    = maybe def (either error id . readT) $ getAttribute name attribs

(.:?) :: ReadT a => ParsedAttributes -> Text -> a -> a
attribs .:? name =
    flip fromMaybe (fmap (either error id . readT) $ getAttribute name attribs)

extractFlag :: Text -> ParsedAttributes -> Bool
extractFlag = extractAttributeOrDefault False

(.!) :: ParsedAttributes -> Text -> Bool
(.!) = flip extractFlag

class ReadT a where
    readT :: Maybe Text -> Either String a
    default readT :: Read a => Maybe Text -> Either String a
    readT = maybe (Left "empty") (Right . read . Text.unpack)

instance ReadT Word8

instance ReadT Bool

instance ReadT Text where
    readT = maybe (Left "empty") Right

instance ReadT Position where
    readT = undefined

instance ReadT FilePath
