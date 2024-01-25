{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Pattern.Bool (
    test_patternSynonyms,
    test_conjunction_splitters,
) where

import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Syntax.ParsedKore.Internalise (symb)
import Data.ByteString (ByteString)
import Test.Booster.Fixture
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
test_patternSynonyms :: TestTree
test_patternSynonyms =
    testGroup
        "Pattern synonyms for predicate symbols should match parsed symbol applications"
        [ testPatternAndBool
        , testPatternNotBool
        , testPatternEqualsBool
        , testPatternEqualsInt
        , testPatternNEqualsInt
        , testPatternEqualsK
        , testPatternNEqualsK
        , testPatternSetIn
        ]

testPatternAndBool :: TestTree
testPatternAndBool =
    testCase "_andBool_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'Unds'andBool'Unds'{}(SortBool{}, SortBool{}) : SortBool{} [function{}(), hook{}("BOOL.and"), smt-hook{}("and"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool, TrueBool]
         in case appliedParsedSymbol of
                match@(AndBool _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNotBool :: TestTree
testPatternNotBool =
    testCase "notBool_" $
        let parsedSymbol =
                [symb| hooked-symbol LblnotBool'Unds'{}(SortBool{}) : SortBool{} [function{}(), hook{}("BOOL.not"), smt-hook{}("not"),total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool]
         in case appliedParsedSymbol of
                match@(NotBool _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternEqualsBool :: TestTree
testPatternEqualsBool =
    testCase "_==Bool_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsEqls'Bool'Unds'{}(SortBool{}, SortBool{}) : SortBool{} [function{}(), hook{}("BOOL.eq"), smt-hook{}("="), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool, TrueBool]
         in case appliedParsedSymbol of
                match@(EqualsBool _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternEqualsInt :: TestTree
testPatternEqualsInt =
    testCase "_==Int_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsEqls'Int'Unds'{}(SortInt{}, SortInt{}) : SortBool{} [function{}(), hook{}("INT.eq"), smt-hook{}("="), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(EqualsInt _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNEqualsInt :: TestTree
testPatternNEqualsInt =
    testCase "_=/=Int_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsSlshEqls'Int'Unds'{}(SortInt{}, SortInt{}) : SortBool{} [function{}(), hook{}("INT.ne"), smt-hook{}("distinct"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"] of
                match@(NEqualsInt _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

dvInt :: ByteString -> Term
dvInt = DomainValue (SortApp "SortInt" [])

testPatternEqualsK :: TestTree
testPatternEqualsK =
    testCase "_==K_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsEqls'K'Unds'{}(SortK{}, SortK{}) : SortBool{} [function{}(), hook{}("KEQUAL.eq"), smt-hook{}("="), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(EqualsK _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNEqualsK :: TestTree
testPatternNEqualsK =
    testCase "_=/=K_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsSlshEqls'K'Unds'{}(SortK{}, SortK{}) : SortBool{} [function{}(), hook{}("KEQUAL.ne"), smt-hook{}("distinct"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(NEqualsK _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternSetIn :: TestTree
testPatternSetIn =
    testCase "_Set:in_" $
        let parsedSymbol =
                [symb| hooked-symbol LblSet'Coln'in{}(SortKItem{}, SortSet{}) : SortBool{} [function{}(), hook{}("SET.in"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"] -- these arguments do not make sense here, but that's not what we are testing for
         in case appliedParsedSymbol of
                match@(SetIn _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

--------------------------------------------------------------------------------

test_conjunction_splitters :: TestTree
test_conjunction_splitters =
    testGroup
        "Conjunction splitting functions work as expected"
        [testSplitAndBool, testSplitBoolPredicates]

testSplitAndBool :: TestTree
testSplitAndBool =
    testGroup "splitAndBool splits everything" $
        testCase
            "A concrete conjunct is split"
            ( splitAndBools
                (Predicate (AndBool TrueBool TrueBool))
                @?= [Predicate TrueBool, Predicate TrueBool]
            )
            : commonTestCases splitAndBools

testSplitBoolPredicates :: TestTree
testSplitBoolPredicates =
    testGroup "splitBoolPredicates leaves concrete conjunctions lumped together" $
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
