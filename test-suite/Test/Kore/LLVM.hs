{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Test.Kore.LLVM (
    test_llvmLoad,
) where

import Data.List (isPrefixOf, isSuffixOf)
import GHC.IO.Exception
import System.Environment
import System.FilePath
import System.Process
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Kore.LLVM.Internal

-- A prerequisite for all tests in this suite is that a fixed K
-- definition was compiled for LLVM in 'c' mode to produce a dynamic
-- library, and that library is available under 'test/llvm.dl'

definition, kompiledPath, dlPath :: FilePath
definition = "test/llvm/llvm.k"
kompiledPath = "test/llvm-kompiled"
dlPath = kompiledPath </> "interpreter"

-- TODO integrate with hedgehog: https://hackage.haskell.org/package/hspec-hedgehog

test_llvmLoad :: IO TestTree
test_llvmLoad = testSpec "LLVM simplification library tests" llvmSpec


llvmSpec :: Spec
llvmSpec =
    beforeAll_ runKompile $ do
        describe "Load an LLVM simplification library" $ do
            it "fails to load a non-existing library" $
                withDLib "does/not/exist.dl" mkAPI `shouldThrow`
                    \IOError{ioe_description = msg} ->
                        "dlopen: does/not/exist" `isPrefixOf` msg
                            && "No such file or directory" `isSuffixOf` msg
            it ("loads a valid library from " <> dlPath) $ do
                pendingWith "need compilation setup and env. variable"

runKompile :: IO ()
runKompile = do
    putStrLn "[Info] Compiling definition to produce a dynamic LLVM library.."
    -- FIXME why do I see this more than once?
    callProcess
        "kompile"
        [ definition
        , "--llvm-kompile-type"
        , "c"
        , "-o"
        , kompiledPath
        ]
