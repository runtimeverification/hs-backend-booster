{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Syntax.Json.Internalise (
    test_internalise,
) where

import Control.Monad.Trans.Except
import Data.Coerce
import Data.Set qualified as Set
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
        [ testPredicates
        ]

testPredicates :: TestTree
testPredicates =
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
            Right (Set.singleton expected, mempty, mempty, []) @=? runExcept (internalise pat)

    expectUnsupported description pat expected =
        testCase ("Unsupported: " <> description) $
            Right (mempty, mempty, mempty, [expected]) @=? runExcept (internalise pat)

-- syntax equivalents of sorts in testDefinition
someSort', boolSort', intSort' :: Syntax.Sort
someSort' = Syntax.SortApp (Id "SomeSort") []
boolSort' = Syntax.SortApp (Id "SortBool") []
intSort' = Syntax.SortApp (Id "SortInt") []
