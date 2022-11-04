{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

-}

module Kore.Syntax.ParsedKore (
    -- * Parsing
    parseKoreDefinition,
    parseKorePattern
) where

import Data.Text (Text)

import Kore.Syntax.Json.Base
import Kore.Syntax.ParsedKore.Base
import Kore.Syntax.ParsedKore.Parser qualified as Parser

-- Parsing text

{- | Parse a string representing a Kore definition.

@parseKoreDefinition@ returns a 'KoreDefinition' upon success, or an parse error
message otherwise. The input must contain a valid Kore definition and nothing
else.
-}
parseKoreDefinition ::
    -- | Filename used for error messages
    FilePath ->
    -- | The concrete syntax of a valid Kore definition
    Text ->
    Either String ParsedDefinition
parseKoreDefinition = Parser.parseDefinition

{- | Parse a string representing a Kore pattern.

@parseKorePattern@ returns a 'ParsedPattern' upon success, or an parse error
message otherwise. The input must contain a valid Kore pattern and nothing else.
-}
parseKorePattern ::
    -- | Filename used for error messages
    FilePath ->
    -- | The concrete syntax of a valid Kore pattern
    Text ->
    Either String KorePattern
parseKorePattern = Parser.parsePattern

-- internalising parsed data

-- validates the parsed data and extracts everything we need internally
-- internalise :: ParsedDefinition -> Either [DefinitionError] TypeToBeDefined
