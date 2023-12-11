{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Syntax.Json.Internalise (
    test_internalise,
) where

import Control.Monad.Trans.Except
import Data.Coerce
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Booster.Pattern.Base as Internal
import Booster.Pattern.Bool as Internal
import Booster.Syntax.Json.Internalise
import Kore.Syntax.Json.Types as Syntax hiding (LeftRight (..))
import Test.Booster.Fixture

test_internalise :: TestTree
test_internalise =
    testGroup
        "Internalising patterns"
        [ testBasicPredicates
        , testSubstitutions
        ]

testBasicPredicates :: TestTree
testBasicPredicates =
    testGroup
        "Internalising different kinds of predicates"
        [ shouldBeBool
            "basic boolean predicate (simply false)"
            (KJEquals boolSort' someSort' (KJDV boolSort' "true") (KJDV boolSort' "false"))
            (coerce FalseBool)
        , -- tricky cases
          let notAndCeil =
                KJNot someSort' . KJAnd someSort' $
                    [ KJCeil boolSort' someSort' $ KJDV boolSort' "true"
                    , KJEquals boolSort' someSort' (KJDV boolSort' "true") (KJDV boolSort' "false")
                    ]
           in expectUnsupported
                "conjunction under a not with ceil and an equation"
                notAndCeil
                notAndCeil
        ]
  where
    internaliseAll = internalisePredicates DisallowAlias IgnoreSubsorts Nothing testDefinition
    internalise = internaliseAll . (: [])

    shouldBeBool name pat expected =
        testCase name $
            Right (InternalisedPredicates (Set.singleton expected) mempty mempty [])
                @=? runExcept (internalise pat)

    expectUnsupported description pat expected =
        testCase ("Unsupported: " <> description) $
            Right (InternalisedPredicates mempty mempty mempty [expected])
                @=? runExcept (internalise pat)

testSubstitutions :: TestTree
testSubstitutions =
    testGroup
        "Recognising and checking substitutions"
        [ test
            "Basic substitution equation"
            ( "X" .==> dv' "something")
            (hasSubstitution [("X", someSort, DomainValue someSort "something")])
        , test
            "Several substitutions"
            (equations'
                [  "X" .==> dv' "something"
                ,  "Y" .==> dv' "something-else"
                ])
            (hasSubstitution
                [ ("X", someSort, DomainValue someSort "something")
                , ("Y", someSort, DomainValue someSort "something-else")
                ])
        , let varX = Var (Variable someSort "X")
          in test
                 "X => f(X) is not a substitution"
                 ( "X" .==> KJApp (Id "f1") [] [KJEVar (Id "X") someSort'])
                 (hasEquations [(varX, SymbolApplication f1 [] [varX])])
        ]
  where
    test name syntax expected =
        testCase name $
            Right expected @=? internalise [syntax]
    internalise =
        runExcept . internalisePredicates DisallowAlias IgnoreSubsorts Nothing testDefinition

    hasSubstitution triplets =
        let expectedSubstitution =
                Map.fromList [ (Variable s v, t)  | (v, s, t) <- triplets ]
         in InternalisedPredicates mempty mempty expectedSubstitution mempty

    hasEquations pairs =
        let expectedPreds =
                Set.fromList [ t ==. t' | (t, t') <- pairs ]
         in InternalisedPredicates expectedPreds mempty mempty mempty

(.==>) :: Text -> Syntax.KorePattern -> Syntax.KorePattern
v .==> t' = KJEquals someSort' someSort' (KJEVar (Id v) someSort') t'

dv' :: Text -> Syntax.KorePattern
dv' = KJDV someSort'

equations' :: [Syntax.KorePattern] -> Syntax.KorePattern
equations' = KJAnd someSort'

-- syntax equivalents of sorts in testDefinition
someSort', boolSort', intSort' :: Syntax.Sort
someSort' = Syntax.SortApp (Id "SomeSort") []
boolSort' = Syntax.SortApp (Id "SortBool") []
intSort' = Syntax.SortApp (Id "SortInt") []

-- produces a K-equals predicate
(==.) :: Internal.Term -> Internal.Term -> Internal.Predicate
t ==. t' = Predicate $ SymbolApplication eqK [] [inDotK t, inDotK t']
  where
    inDotK :: Internal.Term -> Internal.Term
    inDotK t = SymbolApplication kseq [] [t, SymbolApplication dotk [] []]
