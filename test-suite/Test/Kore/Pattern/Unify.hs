{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Kore.Pattern.Unify (
    test_unification,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import Kore.Definition.Base
import Kore.Definition.Attributes.Base
import Kore.Pattern.Base
import Kore.Pattern.Unify

test_unification :: TestTree
test_unification =
    testGroup
        "Unification"
        [ constructors
        ]

constructors :: TestTree
constructors =
    testGroup
        "from story description"
        [ test
              "same constructors, one variable argument"
              (app "con1" someSort [someSort] [var "X" someSort])
              (app "con1" someSort [someSort] [var "Y" someSort])
              (success [("X", someSort, var "Y" someSort)])
        , test
              "same constructors, same argument"
              (app "con1" someSort [someSort] [var "X" someSort])
              (app "con1" someSort [someSort] [var "X" someSort])
              (success [])
        , let v1 = var "X" someSort
              v2 = var "X" anotherSort
           in test
                  "same constructors, argument variables differ in sorts"
                  (app "con1" someSort [someSort] [v1])
                  (app "con1" someSort [someSort] [v2])
              (failed $ DifferentSorts v1 v2)
        , test
              "same constructor, var./term argument"
              (app "con1" someSort [someSort] [var "X" someSort])
              (app "con1" someSort [someSort] [app "f1" someSort [someSort] [var "Y" someSort]])
              (success [("X", someSort, app "f1" someSort [someSort] [var "Y" someSort])])
        , let t1 = app "con1" someSort [someSort] [var "X" someSort]
              t2 = app "con2" someSort [someSort] [var "Y" someSort]
           in test "different constructors" t1 t2 $ failed (DifferentSymbols t1 t2)
        , let t1 = app "f1" someSort [someSort] [var "X" someSort]
              t2 = app "f1" someSort [someSort] [var "Y" someSort]
           in test "Functions" t1 t2 $ remainder [(t1, t2)]
        ]

----------------------------------------
someSort, anotherSort :: Sort
someSort = SortApp "SomeSort" []
anotherSort = SortApp "AnotherSort" []

app :: SymbolName -> Sort -> [Sort] -> [Term] -> Term
app n resultSort argSorts args = SymbolApplication resultSort argSorts n args

var :: VarName -> Sort -> Term
var variableName variableSort = Var $ Variable{variableSort, variableName}

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
test :: String -> Term -> Term -> UnificationResult -> TestTree
test name term1 term2 expected =
    testCase name $ unifyTerms stdDef term1 term2 @?= expected
  where
    stdDef =
        dummyDefinition
            [simpleSortInfo someSort, anotherSort `subsortOf` someSort]
            [ constructor "con1" someSort [someSort]
            , constructor "con2" someSort [someSort]
            , constructor "con3" someSort [someSort, someSort]
            , constructor "con4" anotherSort [someSort, anotherSort]
            , function "f1" someSort [someSort]
            , partialFunction "f2" someSort [someSort]
            ]

    simpleSortInfo (SortApp n []) = (n, (SortAttributes{argCount = 0}, Set.singleton n))
    simpleSortInfo other = error $ "Sort info: " <> show other <> " not supported"

    (SortApp sub []) `subsortOf` (SortApp super []) =
        (sub, (SortAttributes{argCount = 0}, Set.fromList [sub, super] ))
    other1 `subsortOf` other2 =
        error $ "subSortOf: " <> show (other1, other2) <> " not supported"

    constructor n sort argSorts =
        (n, (SymbolAttributes False False True, symSort sort argSorts))
    function n sort argSorts =
        (n, (SymbolAttributes True True False, symSort sort argSorts))
    partialFunction n sort argSorts =
        (n, (SymbolAttributes True False False, symSort sort argSorts))

    symSort s argSorts = SymbolSort s argSorts

dummyDefinition ::
    [(SortName, (SortAttributes, Set SortName))] ->
    [(SymbolName, (SymbolAttributes, SymbolSort))] ->
    KoreDefinition
dummyDefinition sorts symbols =
    KoreDefinition
    { attributes = DefinitionAttributes
    , modules = Map.singleton "AMODULE" ModuleAttributes
    , sorts = Map.fromList sorts
    , symbols = Map.fromList symbols
    , aliases = Map.empty
    , rewriteTheory = Map.empty
    }
