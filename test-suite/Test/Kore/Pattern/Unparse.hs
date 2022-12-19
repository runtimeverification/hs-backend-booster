module Test.Kore.Pattern.Unparse (
    test_unparse,
) where

import Test.Tasty
import Test.Tasty.HUnit

import Kore.Pattern.Base

test_unparse :: TestTree
test_unparse = 
    testGroup
        "Term unparsing"
        []
