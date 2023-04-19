{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Parsing attributes from @ParsedAttributes@ to different internal
types. The required attribute names and parsers for the expected
values are hard-wired.
-}
module Booster.Definition.Attributes.Reader (
    HasAttributes (..),
    readLocation,
) where

import Control.Applicative (liftA2)
import Control.Monad.Extra (whenM)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Char (isDigit)
import Data.Coerce (Coercible, coerce)
import Data.Kind
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.Read (readEither)
import Text.Regex.PCRE

import Booster.Definition.Attributes.Base (
    AxiomAttributes (AxiomAttributes),
    Concrete (..),
    DefinitionAttributes (..),
    FileSource (..),
    Flag (Flag),
    Location (Location),
    ModuleAttributes (..),
    Position (Position),
    Priority,
    SortAttributes (..),
    SymbolAttributes (SymbolAttributes),
    SymbolType (
        Constructor,
        PartialFunction,
        SortInjection,
        TotalFunction
    ),
 )
import Booster.Syntax.ParsedKore.Base
import Kore.Syntax.Json.Types (Id (..))

{- | A class describing all attributes we want to extract from parsed
 entities
-}
class HasAttributes ty where
    type Attributes ty :: Type

    mkAttributes :: ty -> Except Text (Attributes ty)

instance HasAttributes ParsedDefinition where
    type Attributes ParsedDefinition = DefinitionAttributes

    mkAttributes _ = pure DefinitionAttributes

instance HasAttributes ParsedModule where
    type Attributes ParsedModule = ModuleAttributes

    mkAttributes _ = pure ModuleAttributes

instance HasAttributes ParsedAxiom where
    type Attributes ParsedAxiom = AxiomAttributes

    mkAttributes ParsedAxiom{attributes} =
        AxiomAttributes
            <$> readLocation attributes
            <*> readPriority attributes
            <*> (attributes .:? "label")
            <*> (attributes .:? "simplification")
            <*> (fromMaybe False <$> (attributes .:? "preserves-definedness"))
            <*> (fromMaybe (Concrete Nothing) <$> (attributes .:? "concrete"))

sourceName
    , locationName ::
        Text
sourceName = "org'Stop'kframework'Stop'attributes'Stop'Source"
locationName = "org'Stop'kframework'Stop'attributes'Stop'Location"

readLocation :: ParsedAttributes -> Except Text (Maybe Location)
readLocation attributes = do
    file <- attributes .:? sourceName
    case file of
        Nothing -> pure Nothing
        Just f -> Just . Location f <$> attributes .: locationName

{- | Reads 'priority' and 'simplification' attributes (with -optional-
   number in [0..200]) and 'owise' flag, returning either the given
   priority or lowest priority if 'owise'. Reports an error if more
   than one of these is present, defaults to 50 if none.
-}
readPriority :: ParsedAttributes -> Except Text Priority
readPriority attributes = do
    priority <-
        fmap ("priority",) <$> attributes .:? "priority"
    simplification <-
        fmap ("simplification",) <$> attributes .:? "simplification"
    owise <-
        fmap ("owise",) . (\b -> if b then Just maxBound else Nothing) <$> attributes .! "owise"
    case catMaybes [simplification, priority, owise] of
        [] ->
            pure 50
        [(_, p)] ->
            pure p
        more ->
            throwE $ "Several priorities given: " <> Text.intercalate "," (map fst more)

instance HasAttributes ParsedSymbol where
    type Attributes ParsedSymbol = SymbolAttributes

    mkAttributes ParsedSymbol{name, attributes} = do
        isInjctn <- attributes .! "sortInjection"
        let symbolType = do
                isConstr <- attributes .! "constructor"
                isFunctn <- attributes .! "function"
                isTotal <- attributes .! "total" <||> attributes .! "functional"
                case (isConstr, isInjctn, isFunctn, isTotal) of
                    (True, _, _, _) -> pure Constructor
                    (_, True, _, _) -> pure SortInjection
                    (_, _, _, True) -> pure TotalFunction
                    (_, _, True, _) -> pure PartialFunction
                    _other ->
                        throwE $ "Invalid symbol type '" <> name.getId <> "', attributes: " <> Text.pack (show attributes)
            isIdem = do
                whenM (attributes .! "sortInjection" <&&> attributes .! "idem") $
                    throwE $
                        "Sort injection '" <> name.getId <> "' cannot be idempotent."
                attributes .! "idem"
            isAssoc = do
                whenM (attributes .! "sortInjection" <&&> attributes .! "assoc") $
                    throwE $
                        "Sort injection '" <> name.getId <> "' cannot be associative."
                attributes .! "assoc"
        SymbolAttributes
            <$> symbolType
            <*> isIdem
            <*> isAssoc
            <*> (coerce <$> (attributes .! "macro" <||> attributes .! "alias'Kywd'"))

instance HasAttributes ParsedSort where
    type Attributes ParsedSort = SortAttributes

    mkAttributes ParsedSort{sortVars} =
        pure SortAttributes{argCount = length sortVars}

----------------------------------------

readError :: Text -> String -> Text
readError name msg = name <> " could not be read: " <> Text.pack msg

extractAttribute :: ReadT a => Text -> ParsedAttributes -> Except Text a
extractAttribute name attribs =
    (maybe (throwE notFound) pure $ getAttribute name attribs)
        >>= except . first (readError name) . readT
  where
    notFound = name <> " not found in attributes."

(.:) :: ReadT a => ParsedAttributes -> Text -> Except Text a
(.:) = flip extractAttribute

extractAttributeOrDefault :: ReadT a => a -> Text -> ParsedAttributes -> Except Text a
extractAttributeOrDefault def name attribs =
    maybe (pure def) (either (throwE . readError name) pure . readT) $ getAttribute name attribs

(.:?) :: forall a. ReadT a => ParsedAttributes -> Text -> Except Text (Maybe a)
attribs .:? name =
    except . first (readError name) . mapM readT $ getAttribute name attribs

extractFlag :: Coercible Bool b => Text -> ParsedAttributes -> Except Text b
extractFlag = coerce . extractAttributeOrDefault False

(.!) :: Coercible Bool b => ParsedAttributes -> Text -> Except Text b
(.!) = flip extractFlag

infix 5 .!
infix 5 .:?
infix 5 .:

-- see GHC.Utils.Misc:
(<&&>), (<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)
(<||>) = liftA2 (||)
infixr 3 <&&>
infixr 2 <||>

----------------------------------------

-- | Type class providing safe readers for different types
class ReadT a where
    readT :: Maybe Text -> Either String a
    default readT :: Read a => Maybe Text -> Either String a
    readT = maybe (Left "empty") (readEither . Text.unpack)

instance ReadT Priority where
    readT Nothing = Right 50 -- HACK to accept `simplification()` from internal modules
    readT (Just "") = Right 50
    readT (Just n)
        | all isDigit (Text.unpack n) = readEither $ "Priority " <> Text.unpack n
        | otherwise = Left $ "invalid priority value " <> show n

-- | Bool instance: presence of the attribute implies 'True'
instance ReadT Bool where
    readT = maybe (Right True) (readEither . Text.unpack)

instance ReadT Text where
    readT = maybe (Left "empty") Right

instance ReadT Concrete where
    readT Nothing = Right $ Concrete Nothing
    readT (Just "") = Right $ Concrete $ Just []
    readT (Just xs) = let split = Text.splitOn "," xs in Concrete . Just <$> mapM readVar split
      where
        readVar str = case Text.splitOn ":" str of
            [name, sort] -> Right (Text.encodeUtf8 name, Text.encodeUtf8 $ Text.replace "{}" "" sort)
            _ -> Left "Invalid variable"

instance ReadT Position where
    readT = maybe (Left "empty position") readLocationType
      where
        readLocationType :: Text -> Either String Position
        readLocationType input =
            case input %%~ locRegex of
                ("", _match, "", [lineStr, columnStr, _, _]) ->
                    Right $ Position (read lineStr) (read columnStr)
                _other ->
                    Left $ show input <> ": garbled location data"

        natRegex, locRegex :: String
        natRegex = "(0|[1-9][0-9]*)"
        locRegex =
            mconcat
                [ "^Location\\("
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , "\\)$"
                ]

-- Strips away the Source(...) constructor that gets printed, if there
-- is one. If there is none, it uses the attribute string as-is.
instance ReadT FileSource where
    readT = maybe (Left "empty file source") readSource
      where
        readSource :: Text -> Either String FileSource
        readSource input =
            case input %%~ "^Source\\((..*)\\)$" of
                ("", _all, "", [file]) ->
                    Right $ FileSource file
                (unmatched, "", "", []) ->
                    Right $ FileSource unmatched
                _other ->
                    Left $ "bad source: " <> show input

-- helper to pin regex match type
(%%~) :: Text -> String -> (String, String, String, [String])
txt %%~ regex = Text.unpack txt =~ regex
