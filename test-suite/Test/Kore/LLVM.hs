{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Test.Kore.LLVM (
    test_llvmSimplification,
) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (pack)
import GHC.IO.Exception
import System.FilePath
import System.Process
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Kore.Definition.Attributes.Base
import Kore.LLVM as LLVM
import Kore.LLVM.Internal as Internal
import Kore.Pattern.Base

-- A prerequisite for all tests in this suite is that a fixed K
-- definition was compiled in LLVM 'c' mode to produce a dynamic
-- library, and is available under 'test/llvm-kompiled/interpreter'

definition, kompiledPath, dlPath :: FilePath
definition = "test/llvm/llvm.k"
kompiledPath = "test/llvm-kompiled"
dlPath = kompiledPath </> "interpreter"

-- TODO integrate with hedgehog: https://hackage.haskell.org/package/hspec-hedgehog

test_llvmSimplification :: IO TestTree
test_llvmSimplification = testSpec "LLVM simplification library tests" llvmSpec

llvmSpec :: Spec
llvmSpec =
    beforeAll_ runKompile $ do
        llvmLoad
        beforeAll loadAPI $ do
            llvmBool

llvmLoad :: Spec
llvmLoad =
    describe "Load an LLVM simplification library" $ do
        it "fails to load a non-existing library" $
            withDLib "does/not/exist.dl" mkAPI
                `shouldThrow` \IOError{ioe_description = msg} ->
                    "dlopen: does/not/exist"
                        `isPrefixOf` msg
                        && "No such file or directory"
                        `isSuffixOf` msg
        it ("loads a valid library from " <> dlPath) $ do
            withDLib dlPath $ \dl -> do
                api <- mkAPI dl
                let testString = "testing, one, two, three"
                s <- api.patt.string.new $ pack testString
                api.patt.dump s
                    `shouldReturn` show testString

llvmBool :: SpecWith Internal.API
llvmBool =
    describe "LLVM boolean simplification" $ do
        it "should leave literal booleans as they are" $ \api -> do
            LLVM.simplifyBool api false `shouldBe` False
            LLVM.simplifyBool api true `shouldBe` True
        it "should be able to compare numbers" $ \api -> do
            LLVM.simplifyBool api equals42 `shouldBe` True
            LLVM.simplifyBool api unequal `shouldBe` False
  where
    -- TODO use hedgehog here

    boolSort = SortApp "SortBool" []
    intSort = SortApp "SortInt" []
    false = DomainValue boolSort "false"
    true = DomainValue boolSort "true"
    fortytwo = DomainValue intSort "42"
    fortyone = DomainValue intSort "41"

    equals42 = SymbolApplication eqInt [] [fortytwo, fortytwo]
    unequal = SymbolApplication eqInt [] [fortyone, fortytwo]

    eqInt = Symbol "Lbl'UndsEqlsEqls'Int'Unds'" [] [intSort, intSort] boolSort (SymbolAttributes TotalFunction False False)

runKompile :: IO ()
runKompile = do
    putStrLn "[Info] Compiling definition to produce a dynamic LLVM library.."
    callProcess
        "kompile"
        [ definition
        , "--llvm-kompile-type"
        , "c"
        , "-o"
        , kompiledPath
        ]

loadAPI :: IO API
loadAPI = withDLib dlPath mkAPI
