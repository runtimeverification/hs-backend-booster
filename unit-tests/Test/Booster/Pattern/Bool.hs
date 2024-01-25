{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Pattern.Bool (
    test_splitAndBool,
    test_splitBoolPredicates,
) where

import Booster.Pattern.Base
import Booster.Pattern.Bool
import Test.Booster.Fixture
import Test.Tasty
import Test.Tasty.HUnit

test_splitAndBool :: [TestTree]
test_splitAndBool =
    testCase
        "A concrete conjunct is split"
        ( splitAndBools
            (Predicate (AndBool TrueBool TrueBool))
            @?= [Predicate TrueBool, Predicate TrueBool]
        )
        : commonTestCases splitAndBools

test_splitBoolPredicates :: [TestTree]
test_splitBoolPredicates =
    testCase
        "A concrete conjunct is left NOT split"
        ( splitBoolPredicates
            (Predicate (AndBool TrueBool TrueBool))
            @?= [Predicate (AndBool TrueBool TrueBool)]
        )
        : commonTestCases splitBoolPredicates

commonTestCases :: (Predicate -> [Predicate]) -> [TestTree]
commonTestCases splitter =
    [ testCase
        "A partially symbolic conjunct is split"
        ( splitter
            (Predicate (AndBool (Var (Variable boolSort "X")) TrueBool))
            @?= [Predicate (Var (Variable boolSort "X")), Predicate TrueBool]
        )
    , testCase
        "A fully symbolic conjunct is split"
        ( splitter
            (Predicate (AndBool (Var (Variable boolSort "X")) (Var (Variable boolSort "X"))))
            @?= [Predicate (Var (Variable boolSort "X")), Predicate (Var (Variable boolSort "X"))]
        )
    ]
