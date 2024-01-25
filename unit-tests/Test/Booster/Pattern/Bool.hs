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
                [symb| hooked-symbol Lbl'Unds'andBool'Unds'{}(SortBool{}, SortBool{}) : SortBool{} [bfunction{}(), functional{}(), hook{}("BOOL.and"), smt-hook{}("and"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool, TrueBool]
         in case appliedParsedSymbol of
                match@(AndBool _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNotBool :: TestTree
testPatternNotBool =
    testCase "notBool_" $
        let parsedSymbol =
                [symb| hooked-symbol LblnotBool'Unds'{}(SortBool{}) : SortBool{} [boolOperation{}(), format{}("%cnotBool%r %1"), function{}(), functional{}(), hook{}("BOOL.not"), klabel{}("notBool_"), latex{}("\\neg_{\\scriptstyle\\it Bool}{#1}"), left{}(), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(1100,19,1100,179)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(Lbl'Unds'orElseBool'Unds'{}(),Lbl'Unds'orBool'Unds'{}(),Lbl'UndsEqlsSlshEqls'Bool'Unds'{}(),Lbl'Unds'andThenBool'Unds'{}(),Lbl'Unds'impliesBool'Unds'{}(),Lbl'UndsEqlsEqls'Bool'Unds'{}(),Lbl'Unds'andBool'Unds'{}(),Lbl'Unds'xorBool'Unds'{}()), right{}(), smt-hook{}("not"), symbol'Kywd'{}(), terminals{}("10"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool]
         in case appliedParsedSymbol of
                match@(NotBool _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternEqualsBool :: TestTree
testPatternEqualsBool =
    testCase "_==Bool_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsEqls'Bool'Unds'{}(SortBool{}, SortBool{}) : SortBool{} [comm{}(), format{}("%1 %c==Bool%r %2"), function{}(), functional{}(), hook{}("BOOL.eq"), klabel{}("_==Bool_"), left{}(Lbl'UndsEqlsSlshEqls'Bool'Unds'{}(),Lbl'UndsEqlsEqls'Bool'Unds'{}()), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(1108,19,1108,126)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(), right{}(), smt-hook{}("="), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [TrueBool, TrueBool]
         in case appliedParsedSymbol of
                match@(EqualsBool _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternEqualsInt :: TestTree
testPatternEqualsInt =
    testCase "_==Int_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsEqls'Int'Unds'{}(SortInt{}, SortInt{}) : SortBool{} [comm{}(), format{}("%1 %c==Int%r %2"), function{}(), functional{}(), hook{}("INT.eq"), klabel{}("_==Int_"), latex{}("{#1}\\mathrel{{=}{=}_{\\scriptstyle\\it Int}}{#2}"), left{}(), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(1308,19,1308,173)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(), right{}(), smt-hook{}("="), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(EqualsInt _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNEqualsInt :: TestTree
testPatternNEqualsInt =
    testCase "_=/=Int_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsSlshEqls'Int'Unds'{}(SortInt{}, SortInt{}) : SortBool{} [comm{}(), format{}("%1 %c=/=Int%r %2"), function{}(), functional{}(), hook{}("INT.ne"), klabel{}("_=/=Int_"), latex{}("{#1}\\mathrel{{=}{/}{=}_{\\scriptstyle\\it Int}}{#2}"), left{}(), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(1309,19,1309,184)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(), right{}(), smt-hook{}("distinct"), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
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
                [symb| hooked-symbol Lbl'UndsEqlsEqls'K'Unds'{}(SortK{}, SortK{}) : SortBool{} [comm{}(), equalEqualK{}(), format{}("%1 %c==K%r %2"), function{}(), functional{}(), hook{}("KEQUAL.eq"), klabel{}("_==K_"), latex{}("{#1}\\mathrel{=_K}{#2}"), left{}(Lbl'UndsEqlsSlshEqls'K'Unds'{}(),Lbl'UndsEqlsEqls'K'Unds'{}()), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(2259,19,2259,165)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(Lbl'Unds'orElseBool'Unds'{}(),Lbl'Unds'orBool'Unds'{}(),Lbl'UndsEqlsSlshEqls'Bool'Unds'{}(),Lbl'Unds'andThenBool'Unds'{}(),Lbl'Unds'impliesBool'Unds'{}(),Lbl'UndsEqlsEqls'Bool'Unds'{}(),Lbl'Unds'andBool'Unds'{}(),LblnotBool'Unds'{}(),Lbl'Unds'xorBool'Unds'{}()), right{}(), smt-hook{}("="), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(EqualsK _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternNEqualsK :: TestTree
testPatternNEqualsK =
    testCase "_=/=K_" $
        let parsedSymbol =
                [symb| hooked-symbol Lbl'UndsEqlsSlshEqls'K'Unds'{}(SortK{}, SortK{}) : SortBool{} [comm{}(), format{}("%1 %c=/=K%r %2"), function{}(), functional{}(), hook{}("KEQUAL.ne"), klabel{}("_=/=K_"), latex{}("{#1}\\mathrel{\\neq_K}{#2}"), left{}(Lbl'UndsEqlsEqls'K'Unds'{}(),Lbl'UndsEqlsSlshEqls'K'Unds'{}()), notEqualEqualK{}(), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(2260,19,2260,179)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(Lbl'Unds'orElseBool'Unds'{}(),Lbl'Unds'orBool'Unds'{}(),Lbl'UndsEqlsSlshEqls'Bool'Unds'{}(),Lbl'Unds'andThenBool'Unds'{}(),Lbl'Unds'impliesBool'Unds'{}(),Lbl'UndsEqlsEqls'Bool'Unds'{}(),Lbl'Unds'andBool'Unds'{}(),LblnotBool'Unds'{}(),Lbl'Unds'xorBool'Unds'{}()), right{}(), smt-hook{}("distinct"), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
            appliedParsedSymbol = SymbolApplication parsedSymbol [] [dvInt "1", dvInt "1"]
         in case appliedParsedSymbol of
                match@(NEqualsK _ _) -> match @?= appliedParsedSymbol
                unexpected -> assertFailure ("pattern synonym matched unexpected symbol application: " <> show unexpected)

testPatternSetIn :: TestTree
testPatternSetIn =
    testCase "_Set:in_" $
        let parsedSymbol =
                [symb| hooked-symbol LblSet'Coln'in{}(SortKItem{}, SortSet{}) : SortBool{} [format{}("%1 %cin%r %2"), function{}(), functional{}(), hook{}("SET.in"), klabel{}("Set:in"), left{}(), org'Stop'kframework'Stop'attributes'Stop'Location{}("Location(777,19,777,102)"), org'Stop'kframework'Stop'attributes'Stop'Source{}("Source(/home/jost/work/RV/code/evm-semantics-with-deps/.build/usr/lib/kevm/kframework/include/kframework/builtin/domains.md)"), priorities{}(), right{}(), symbol'Kywd'{}(), terminals{}("010"), total{}()] |]
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
