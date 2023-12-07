{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

-}
module Test.Booster.Syntax.Json.Internalise (
    test_internalise,
) where

import Control.Monad.Trans.Except
import Data.Coerce
import Test.Tasty
import Test.Tasty.HUnit

import Booster.Pattern.Base as Internal
import Booster.Pattern.Bool as Internal
import Booster.Syntax.Json.Internalise
import Kore.Syntax.Json.Types as Syntax hiding (LeftRight(..))
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
        [ shouldWork
            "basic boolean predicate (simply false)"
            (KJEquals boolSort' someSort' (KJDV boolSort' "true") (KJDV boolSort' "false"))
            (BoolPred . coerce $ FalseBool )
        , -- tricky cases
          let unsupported =
                  KJNot someSort' . KJAnd someSort' $
                      [ KJCeil boolSort' someSort' $ KJDV boolSort' "true"
                      , KJEquals boolSort' someSort' (KJDV boolSort' "true") (KJDV boolSort' "false")
                      ]
           in shouldWork
                  "Unsupported conjunction under a not with ceil and an equation"
                  unsupported
                  (UnsupportedPred unsupported)
        ]
  where
    internalise = internalisePredicate DisallowAlias IgnoreSubsorts Nothing testDefinition
    shouldWork name pat expected =
        testCase name $ Right expected @=? runExcept (internalise pat)

-- syntax equivalents of sorts in testDefinition
someSort', boolSort', intSort' :: Syntax.Sort
someSort' = Syntax.SortApp (Id "SomeSort") []
boolSort' = Syntax.SortApp (Id "SortBool") []
intSort' =  Syntax.SortApp (Id "SortInt") []
