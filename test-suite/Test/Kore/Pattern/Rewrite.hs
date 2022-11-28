{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Kore.Pattern.Rewrite (
    test_rewrite,
) where

import Control.Monad.Trans.Except
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base
import Kore.Pattern.Rewrite
import Test.Kore.Fixture

test_rewrite :: TestTree
test_rewrite =
    testGroup
        "Rewriting"
        [ errorCases
        , rewriteSuccess
        , unifyNotMatch
        , definednessUnclear
        , rewriteStuck
        , rulePriority
        ]

----------------------------------------

def :: KoreDefinition
def =
    testDefinition
        { rewriteTheory =
            mkTheory
                [
                    ( TopSymbol "con1"
                    ,
                        [
                            ( 42
                            , termInKCell "RuleVar" (app con1 [varX])
                            , termInKCell "RuleVar" (app f1 [varX])
                            , Just "con1-f1"
                            )
                        ]
                    )
                ,
                    ( TopSymbol "con3"
                    ,
                        [
                            ( 42
                            , termInKCell "RuleVar" (app con3 [dv someSort "otherThing", varY])
                            , termInKCell "RuleVar" (app con1 [dv someSort "somethingElse"])
                            , Just "con3-con1"
                            )
                        ]
                    )
                ]
        }
  where
    varX = var "X" someSort
    varY = var "Y" someSort

termInKCell :: Text -> Term -> Pattern
termInKCell varName = flip Pattern [] . withinKCell varName

-- indexing only works with a K cell. For realistic test, we also use
-- an injection into 'KItem'.
withinKCell :: Text -> Term -> Term
withinKCell restVar term =
    app kCell [app kseq [injKItem term, var restVar kItemSort]]

kCell, kseq :: Symbol
kCell =
    Symbol
        { name = "Lbl'-LT-'k'-GT-'"
        , resultSort = kSort -- bogus
        , argSorts = [kSort]
        , attributes = asConstructor
        }
kseq =
    Symbol
        { name = "kseq"
        , resultSort = kSort
        , argSorts = [kItemSort, kSort]
        , attributes = asConstructor
        }

kSort, kItemSort :: Sort
kSort = SortApp "SortK" []
kItemSort = SortApp "SortKItem" []

injKItem :: Term -> Term
injKItem = app inj . (: [])

inj :: Symbol
inj =
    Symbol
        { name = "inj"
        , resultSort = SortVar "Target"
        , argSorts = [SortVar "Source"]
        , attributes = SymbolAttributes SortInjection False False
        }

rule :: Maybe Text -> Pattern -> Pattern -> Priority -> RewriteRule
rule label lhs rhs priority =
    RewriteRule
        { lhs
        , rhs
        , attributes = AxiomAttributes{location, priority, label, simplification = False}
        , computedAttributes = ComputedAxiomAttributes False True
        }
  where
    location = Location "no-file" $ Position 0 0

mkTheory :: [(TermIndex, [(Priority, Pattern, Pattern, Maybe Text)])] -> RewriteTheory
mkTheory = Map.map mkPriorityGroups . Map.fromList
  where
    mkPriorityGroups tuples =
        Map.unionsWith (<>)
            [ Map.fromList [(prio, [rule label lhs rhs prio])]
            | (prio, lhs, rhs, label) <- tuples
            ]

d :: Term
d = dv someSort "thing"

----------------------------------------
errorCases
    , rewriteSuccess
    , unifyNotMatch
    , definednessUnclear
    , rewriteStuck
    , rulePriority ::
        TestTree
errorCases =
    testGroup
        "Simple error cases"
        [ testCase "No rules" $
            (termInKCell "Thing" $ app con2 [d]) `failsWith` NoRulesForTerm
            -- , testCase "Index is None" $
            --       (termInKCell "Thing" $ AndTerm (app con1 [d]) (app con2 [d])) `failsWith` TermIndexIsNone
        ]
rewriteSuccess =
    testCase "con1 app rewrites to f1 app" $
        (termInKCell "ConfigVar" $ app con1 [d]) `rewritesTo` (termInKCell "ConfigVar" $ app f1 [d])
unifyNotMatch =
    testCase "Indeterminate case when subject has variables" $ do
        let subst =
                Map.fromList
                    [ (Variable someSort "X", dv someSort "otherThing")
                    , (Variable someSort "Y", d)
                    , (Variable kItemSort "RuleVar", var "ConfigVar" kItemSort)
                    ]
            rule3 =
                rule
                    (Just "con3-con1")
                    (termInKCell "RuleVar" (app con3 [dv someSort "otherThing", var "Y" someSort]))
                    (termInKCell "RuleVar" (app con1 [dv someSort "somethingElse"]))
                    42
        (termInKCell "ConfigVar" $ app con3 [var "X" someSort, d])
            `failsWith` RuleApplicationUncertain
                [UnificationIsNotMatch rule3 subst]
definednessUnclear = dummy
rewriteStuck =
    testCase "con1 app does not unify with con3 app" $
        (termInKCell "ConfigVar" $ app con3 [d])
            `failsWith` NoApplicableRules
rulePriority = dummy

rewritesTo :: Pattern -> Pattern -> IO ()
p1 `rewritesTo` p2 =
    runExcept (rewriteStep def p1) @?= Right (RewriteResult $ NE.singleton p2)

failsWith :: Pattern -> RewriteFailed -> IO ()
failsWith p err =
    runExcept (rewriteStep def p) @?= Left err

----------------------------------------
dummy :: TestTree
dummy = testGroup "FIXME implement me" []
