{- | Utilities for (internalised) definitions and other things

Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Definition.Util (
    Summary (..),
    mkSummary,
    prettySummary,
    decodeLabel,
) where

import Control.DeepSeq (NFData (..))
import Data.ByteString.UTF8 (ByteString)
import Data.ByteString.UTF8 qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base (decodeLabel)
import Kore.Pattern.Index (TermIndex (..))

data Summary = Summary
    { file :: FilePath
    , modNames, sortNames, symbolNames :: [ByteString]
    , subSorts :: Map.Map ByteString [ByteString]
    , axiomCount, preserveDefinednessCount, containAcSymbolsCount :: Int
    , termIndexes :: Map.Map TermIndex [Location]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

mkSummary :: FilePath -> KoreDefinition -> Summary
mkSummary file KoreDefinition{modules, sorts, symbols, rewriteTheory} =
    Summary
        { file
        , modNames = Map.keys modules
        , sortNames = Map.keys sorts
        , symbolNames = Map.keys symbols
        , subSorts = Map.map (Set.toList . snd) sorts
        , axiomCount = length $ concat $ concatMap Map.elems (Map.elems rewriteTheory)
        , preserveDefinednessCount =
            length $
                filter (\rule -> rule.computedAttributes.preservesDefinedness) $
                    concat $
                        concatMap Map.elems (Map.elems rewriteTheory)
        , containAcSymbolsCount =
            length $
                filter (\rule -> rule.computedAttributes.containsAcSymbols) $
                    concat $
                        concatMap Map.elems (Map.elems rewriteTheory)
        , termIndexes =
            Map.map (fmap (.attributes.location) . concat . Map.elems) rewriteTheory
        }

prettySummary :: Summary -> String
prettySummary
    Summary
        { modNames
        , sortNames
        , symbolNames
        , subSorts
        , axiomCount
        , preserveDefinednessCount
        , containAcSymbolsCount
        , termIndexes
        } =
        BS.toString $
            bsUnlines $
                [ list decodeLabel' "Modules" modNames
                , list decodeLabel' "Sorts" sortNames
                , list decodeLabel' "Symbols" symbolNames
                , "Axioms: " <> BS.fromString (show axiomCount)
                , "Axioms preserving definedness: " <> BS.fromString (show preserveDefinednessCount)
                , "Axioms containing AC symbols: " <> BS.fromString (show containAcSymbolsCount)
                ]
                    <> ("Subsorts:" : tableView decodeLabel' subSorts)
                    <> ("Axioms grouped by term index:" : tableView justShow termIndexes')
      where
        tableView :: (x -> ByteString) -> Map.Map ByteString [x] -> [ByteString]
        tableView elemShower table =
            map (("- " <>) . uncurry (list elemShower)) (Map.assocs table)

        list :: (x -> ByteString) -> ByteString -> [x] -> ByteString
        list _ header [] = header <> ": -"
        list f header [x] = header <> ": " <> f x
        list f header xs =
            header
                <> ": "
                <> BS.fromString (show $ length xs)
                <> bsConcat (map (("\n  - " <>) . f) xs)

        decodeLabel' = either error id . decodeLabel

        termIndexes' =
            Map.mapKeys prettyTermIndex termIndexes

        prettyTermIndex Anything = "Anything"
        prettyTermIndex (TopSymbol sym) = decodeLabel' sym
        prettyTermIndex None = "None"

        justShow :: (Show x) => x -> ByteString
        justShow = BS.fromString . show

        bsUnlines = foldr (\a acc -> a <> "\n" <> acc) ""
        bsConcat = foldr (<>) ""
