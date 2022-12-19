{- |
Module      : Kore.Unparse
Copyright   : (c) Runtime Verification, 2018-2022
License     : BSD-3-Clause
-}

{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Kore.Unparse (
    Unparse (..),
    unparseGeneric,
    unparseToText,
    unparseToString,
    renderDefault,
    layoutPrettyUnbounded,
    parameters,
    arguments,
    noArguments,
    attributes,
    parameters',
    arguments',
    argument',
    attributes',
    escapeString,
    escapeStringT,
    escapeChar,
    escapeCharT,
    unparseAssoc',
    unparseConcat',
) where

import Data.Char qualified as Char
import Control.Arrow ((>>>)) -- TODO: remove
import Data.Function ((&))
import Data.Functor.Const
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Void
import Generics.SOP (
    All2,
    Code,
    Generic,
    Proxy (..),
 )
import Generics.SOP qualified as SOP
import Numeric qualified
import Prettyprinter qualified as Pretty 
import Prettyprinter.Render.Text qualified as RenderText
import Prettyprinter.Render.String qualified as RenderString
import Prettyprinter (Doc, SimpleDocStream, LayoutOptions (..))

{- | Class of types that can be rendered in concrete Kore syntax.

@Unparse@ should only be instantiated for types with a corresponding
concrete syntax, i.e. each instance of @Unparse@ should correspond to a
parser in "Kore.Parser.Parser".
-}
class Unparse p where
    -- | Render a type from abstract to concrete Kore syntax.
    unparse :: p -> Doc ann

instance Unparse a => Unparse (Const a child) where
    unparse (Const a) = unparse a

instance Unparse Void where
    unparse = \case {}

{- | Unparse a 'Generic' type with 'unparse'.

/All/ arguments of /all/ constructors must be instances of 'Unparse'; this is
the @'All2' 'Unparse'@ constraint.

Each constructor is unparsed in the following generic way:

- For zero-argument constructors, produce no output ('empty').
- For one-argument constructors, 'unparse' the argument.
- For construtors with more arguments, 'unparse' each argument and combine them
  with 'sep'.

@unparseGeneric@ can be used to quickly implement 'unparse' for types that are
instances of 'Generic'. @unparseGeneric@ is not the default implementation for
all types because it is /excessively/ general. Instances that rely on
@unparseGeneric@ and @unparse2Generic@ should test that these functions
implement the desired behavior, i.e. that they actually produce output that can
be parsed.

See also: 'unparse2Generic'
-}
unparseGeneric :: (Generic a, All2 Unparse (Code a)) => a -> Doc ann
unparseGeneric = unparseGenericWith unparse
{-# INLINE unparseGeneric #-}

unparseGenericWith ::
    (Generic a, All2 Unparse (Code a)) =>
    -- | function to unparse anything
    (forall x. Unparse x => x -> Doc ann) ->
    a ->
    Doc ann
unparseGenericWith helper =
    Pretty.sep . SOP.hcollapse . SOP.hcmap constraint (SOP.mapIK helper) . SOP.from
  where
    constraint = Proxy :: Proxy Unparse
{-# INLINE unparseGenericWith #-}

-- | Serialize an object to 'Text'.
unparseToText :: Unparse p => p -> Text
unparseToText =
    RenderText.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
    . unparse

-- | Serialize an object to 'String'.
unparseToString :: Unparse p => p -> String
unparseToString = renderDefault . unparse

renderDefault :: Doc ann -> String
renderDefault =
    RenderString.renderString
    . Pretty.layoutPretty Pretty.defaultLayoutOptions

parameters :: Unparse p => [p] -> Doc ann
parameters = parameters' . map unparse

-- | Print a list of sort parameters.
parameters' :: [Doc ann] -> Doc ann
parameters' =
    Pretty.braces . Pretty.hsep . Pretty.punctuate Pretty.comma

arguments :: Unparse p => [p] -> Doc ann
arguments = arguments' . map unparse

-- | Print a list of documents as arguments.
arguments' :: [Doc ann] -> Doc ann
arguments' = list Pretty.lparen Pretty.rparen

-- | Print a document as arguments.
argument' :: Doc ann -> Doc ann
argument' = list Pretty.lparen Pretty.rparen . (: [])

-- | Print a list of no arguments.
noArguments :: Doc ann
noArguments = arguments' []

attributes :: Unparse p => [p] -> Doc ann
attributes = attributes' . map unparse

-- | Print a list of documents as attributes.
attributes' :: [Doc ann] -> Doc ann
attributes' = list Pretty.lbracket Pretty.rbracket

-- | Print a list of documents separated by commas in the preferred Kore format.
list ::
    -- | opening list delimiter
    Doc ann ->
    -- | closing list delimiter
    Doc ann ->
    -- | list items
    [Doc ann] ->
    Doc ann
list = listAux Pretty.comma

-- | Print a list of documents separated by commas in the preferred Kore format.
listAux ::
    -- | delimiter
    Doc ann ->
    -- | opening bracket
    Doc ann ->
    -- | closing bracket
    Doc ann ->
    -- | list items
    [Doc ann] ->
    Doc ann
listAux between left right =
    \case
        [] -> left <> right
        xs ->
            xs
                & (Pretty.punctuate between >>> Pretty.vsep)
                & (begin >>> Pretty.nest 4 >>> end >>> Pretty.group)
  where
    begin body = (left <> Pretty.line') <> body
    end body = body <> (Pretty.line' <> right)

-- | Render a 'Doc ann' with indentation and without extra line breaks.
layoutPrettyUnbounded :: Doc ann -> SimpleDocStream ann
layoutPrettyUnbounded =
    Pretty.layoutPretty LayoutOptions{layoutPageWidth = Pretty.Unbounded}

{- | Escape a 'String' for a Kore string literal.

@escapeString@ does not include the surrounding delimiters.
-}
escapeString :: String -> String
escapeString s = foldr escapeCharS "" s

escapeStringT :: Text -> Text
escapeStringT = Text.concatMap escapeCharT

{- | Escape a 'Char' for a Kore character literal.

@escapeChar@ does not include the surrounding delimiters.
-}
escapeChar :: Char -> String
escapeChar c = escapeCharS c ""

escapeCharS :: Char -> ShowS
escapeCharS c
    | c >= '\x20' && c < '\x7F' =
        case Map.lookup c oneCharEscapes of
            Nothing ->
                -- printable 7-bit ASCII
                showChar c
            Just esc ->
                -- single-character escape sequence
                showChar '\\' . showChar esc
    | c < '\x100' = showString "\\x" . zeroPad 2 (showHexCode c)
    | c < '\x10000' = showString "\\u" . zeroPad 4 (showHexCode c)
    | otherwise = showString "\\U" . zeroPad 8 (showHexCode c)
  where
    showHexCode = Numeric.showHex . Char.ord
    zeroPad = padLeftWithCharToLength '0'

escapeCharT :: Char -> Text
escapeCharT c
    | c >= '\x20' && c < '\x7F' =
        case Map.lookup c oneCharEscapes of
            Nothing ->
                -- printable 7-bit ASCII
                Text.singleton c
            Just esc ->
                -- single-character escape sequence
                "\\" <> Text.singleton esc
    | c < '\x100' = "\\x" <> zeroPad 2 (Text.pack $ showHexCode c "")
    | c < '\x10000' = "\\u" <> zeroPad 4 (Text.pack $ showHexCode c "")
    | otherwise = "\\U" <> zeroPad 8 (Text.pack $ showHexCode c "")
  where
    showHexCode = Numeric.showHex . Char.ord
    zeroPad i = Text.justifyRight i '0'

padLeftWithCharToLength :: Char -> Int -> ShowS -> ShowS
padLeftWithCharToLength c i ss =
    showString (replicate (i - length (ss "")) c) . ss

oneCharEscapes :: Map Char Char
oneCharEscapes =
    Map.fromList
        [ ('\\', '\\')
        , ('"', '"')
        , ('\f', 'f')
        , ('\n', 'n')
        , ('\r', 'r')
        , ('\t', 't')
        ]

{- | Unparse an associative binary operator applied to many arguments.

@unparseAssoc'@ avoids creeping indentation.
-}
unparseAssoc' ::
    -- | pattern head
    Doc ann ->
    -- | identity element
    Doc ann ->
    -- | arguments
    [Doc ann] ->
    Doc ann
unparseAssoc' oper ident =
    worker
  where
    worker [] = ident
    worker [x] = x
    worker (x : xs) =
        mconcat
            ( worker' x xs :
              Pretty.line' :
              replicate (length xs) Pretty.rparen
            )

    worker' x [] = Pretty.indent 4 x
    worker' x (y : rest) =
        mconcat
            [ oper <> Pretty.lparen <> Pretty.line'
            , Pretty.indent 4 x <> Pretty.comma <> Pretty.line
            , worker' y rest
            ]

{- | Unparse a concatenation of elements, given the @unit@ and @concat@ symbols.

The children are already unparsed. If they are @element@s of the collection,
they are wrapped by the @element@ symbol.
-}
unparseConcat' ::
    -- | unit symbol
    Pretty.Doc ann ->
    -- | concat symbol
    Pretty.Doc ann ->
    -- | children
    [Pretty.Doc ann] ->
    Pretty.Doc ann
unparseConcat' unitSymbol concatSymbol =
    unparseAssoc' concatSymbol applyUnit
  where
    applyUnit = unitSymbol <> noArguments
