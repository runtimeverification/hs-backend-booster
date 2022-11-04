{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Kore.Syntax.ParsedKore.Parser (
    test_parseFiles,
) where

import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Either
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Kore.Syntax.ParsedKore.Parser

-- Assumption: directory contains textual kore named <name>.kore and
-- text files with potential parse errors <name>.kore.errors, empty if
-- the parse is successful.
testDataDir = "test/parser"

test_parseFiles :: IO TestTree
test_parseFiles = do
    testFiles <- take 32 <$> findByExtension [".errors"] testDataDir
    pure $
        testGroup
            "Parsing textual kore"
            [checkParseErrors f | f <- testFiles]
  where
    checkParseErrors :: FilePath -> TestTree
    checkParseErrors errorFile = goldenVsString name errorFile parseError
      where
        name = "Checking " <> file
        file = takeFileName $ dropExtension errorFile
        kore = dropExtension errorFile
        parseError =
            BS.pack
                . fromLeft ""
                . first (<> "\n")
                . parseDefinition file
                <$> Text.readFile kore
