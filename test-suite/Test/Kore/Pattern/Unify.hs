{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Kore.Pattern.Unify (
    test_unification,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base
import Kore.Pattern.Unify

test_unification :: TestTree
test_unification =
    testGroup
        "Unification"
        [ constructors
        , functions
        , varsAndValues
        , andTerms
        ]

constructors :: TestTree
constructors =
    testGroup
        "Unifying constructors"
        [ test
            "same constructors, one variable argument"
            (app someSort [someSort] "con1" [var "X" someSort])
            (app someSort [someSort] "con1" [var "Y" someSort])
            (success [("X", someSort, var "Y" someSort)])
        , test
            "same constructors, same argument variable"
            (app someSort [someSort] "con1" [var "X" someSort])
            (app someSort [someSort] "con1" [var "X" someSort])
            (success [])
        , let v = var "X" someSort
              d = dv differentSort ""
           in test
                "same constructors, arguments differ in sorts"
                (app someSort [someSort] "con1" [v])
                (app someSort [someSort] "con1" [d])
                (remainder [(v, d)])
        , test
            "same constructor, var./term argument"
            (app someSort [someSort] "con1" [var "X" someSort])
            (app someSort [someSort] "con1" [app someSort [someSort] "f1" [var "Y" someSort]])
            (success [("X", someSort, app someSort [someSort] "f1" [var "Y" someSort])])
        , let t1 = app someSort [someSort] "con1" [var "X" someSort]
              t2 = app someSort [someSort] "con2" [var "Y" someSort]
           in test "different constructors" t1 t2 $ failed (DifferentSymbols t1 t2)
        , let t1 = app someSort [someSort] "con1" [var "X" someSort]
              t2 = app someSort [someSort] "f1" [var "Y" someSort]
           in test "Constructor and function" t1 t2 $ remainder [(t1, t2)]
        ]

functions :: TestTree
functions =
    testGroup
        "Functions (should not unify)"
        [ let f = app someSort [someSort] "f1" [dv someSort ""]
           in test "exact same function (but not unifying)" f f $ remainder [(f, f)]
        , let f1 = app someSort [someSort] "f1" [dv someSort ""]
              f2 = app someSort [someSort] "f2" [dv someSort ""]
           in test "different functions" f1 f2 $ remainder [(f1, f2)]
        ]

varsAndValues :: TestTree
varsAndValues =
    testGroup
        "Variables and Domain Values"
        [ let v = var "X" someSort
           in test "identical variables" v v (success [])
        , let v1 = var "X" someSort
              v2 = var "Y" someSort
           in test "two variables (same sort)" v1 v2 $
                success [("X", someSort, v2)]
        , let v1 = var "X" someSort
              v2 = var "Y" aSubsort
           in test "two variables (v2 subsort v1)" v1 v2 $
                -- TODO could be allowed once subsorts are considered while checking
                remainder [(v1, v2)]
        , let v1 = var "X" aSubsort
              v2 = var "Y" someSort
           in test "two variables (v1 subsort v2)" v1 v2 $
                remainder [(v1, v2)]
        , let v1 = var "X" someSort
              v2 = var "X" differentSort
           in test "same variable name, different sort" v1 v2 $
                failed (VariableConflict (Variable someSort "X") v1 v2)
        , let d1 = dv someSort "1"
              d2 = dv someSort "1"
           in test "same domain values (same sort)" d1 d2 $
                success []
        , let d1 = dv someSort "1"
              d2 = dv someSort "2"
           in test "different domain values (same sort)" d1 d2 $
                failed (DifferentValues d1 d2)
        , let d1 = dv someSort "1"
              d2 = dv differentSort "2"
           in test "different domain values (different sort)" d1 d2 $
                failed (DifferentValues d1 d2)
        , let d1 = dv someSort "1"
              d2 = dv differentSort "1"
           in test "same domain values, different sort" d1 d2 $
                remainder [(d1, d2)]
        , let v = var "X" someSort
              d = dv someSort ""
           in test "var and domain value (same sort)" v d $
                success [("X", someSort, d)]
        , let v = var "X" someSort
              d = dv differentSort ""
           in test "var and domain value (different sort)" v d $
                remainder [(v, d)]
        ]

andTerms :: TestTree
andTerms =
    testGroup
        "And-terms on either side"
        [ let d = dv someSort "a"
              fa = app someSort [someSort] "f1" [d]
              fb = app someSort [someSort] "f1" [dv someSort "b"]
           in test
                  "And-term on the left, remainder returns both pairs"
                  (AndTerm someSort fa fb)
                  d
                  (remainder [(fa, d), (fb, d)])
        , let d = dv someSort "a"
              fa = app someSort [someSort] "f1" [d]
              fb = app someSort [someSort] "f1" [dv someSort "b"]
           in test
                  "And-term on the right, remainder returns both pairs"
                  d
                  (AndTerm someSort fa fb)
                  (remainder [(d, fa), (d, fb)])
        , let da = dv someSort "a"
              db = dv someSort "b"
              ca = app someSort [someSort] "con1" [da]
              cb = app someSort [someSort] "con1" [db]
           in test
                  "And-term on the left, one pair resolves"
                  (AndTerm someSort ca da)
                  cb
                  (remainder [(da, cb), (da, db)])
        , let da = dv someSort "a"
              db = dv someSort "b"
              ca = app someSort [someSort] "con1" [da]
              cb = app someSort [someSort] "con1" [db]
           in test
                  "And-term on the right, one pair resolves"
                  ca
                  (AndTerm someSort cb da)
                  (remainder [(ca, da), (da, db)])
        ]

----------------------------------------
app :: Sort -> [Sort] -> SymbolName -> [Term] -> Term
app = SymbolApplication

var :: VarName -> Sort -> Term
var variableName variableSort = Var $ Variable{variableSort, variableName}

dv :: Sort -> Text -> Term
dv = DomainValue

success :: [(VarName, Sort, Term)] -> UnificationResult
success assocs =
    UnificationSuccess $
        Map.fromList
            [ (Variable{variableSort, variableName}, term)
            | (variableName, variableSort, term) <- assocs
            ]

failed :: FailReason -> UnificationResult
failed = UnificationFailed

remainder :: [(Term, Term)] -> UnificationResult
remainder = UnificationRemainder . NE.fromList

----------------------------------------
-- Test fixture
test :: String -> Term -> Term -> UnificationResult -> TestTree
test name term1 term2 expected =
    testCase name $ unifyTerms testDefinition term1 term2 @?= expected

someSort, aSubsort, differentSort :: Sort
someSort = SortApp "SomeSort" []
aSubsort = SortApp "AnotherSort" []
differentSort = SortApp "DifferentSort" []

testDefinition :: KoreDefinition
testDefinition =
    KoreDefinition
        { attributes = DefinitionAttributes
        , modules = Map.singleton "AMODULE" ModuleAttributes
        , sorts =
            Map.fromList
                [ simpleSortInfo someSort
                , aSubsort `subsortOf` someSort
                , simpleSortInfo differentSort
                ]
        , symbols =
            Map.fromList
                [ constructor "con1" someSort [someSort]
                , constructor "con2" someSort [someSort]
                , constructor "con3" someSort [someSort, someSort]
                , constructor "con4" aSubsort [someSort, aSubsort]
                , function "f1" someSort [someSort]
                , partialFunction "f2" someSort [someSort]
                ]
        , aliases = Map.empty
        , rewriteTheory = Map.empty
        }
  where
    simpleSortInfo (SortApp n []) = (n, (SortAttributes{argCount = 0}, Set.singleton n))
    simpleSortInfo other = error $ "Sort info: " <> show other <> " not supported"

    (SortApp sub []) `subsortOf` (SortApp super []) =
        (sub, (SortAttributes{argCount = 0}, Set.fromList [sub, super]))
    other1 `subsortOf` other2 =
        error $ "subSortOf: " <> show (other1, other2) <> " not supported"

    constructor n sort argSorts =
        (n, (SymbolAttributes False False True, SymbolSort sort argSorts))
    function n sort argSorts =
        (n, (SymbolAttributes True True False, SymbolSort sort argSorts))
    partialFunction n sort argSorts =
        (n, (SymbolAttributes True False False, SymbolSort sort argSorts))
