{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Kore.Fixture (
    module Test.Kore.Fixture,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

import Kore.Definition.Attributes.Base
import Kore.Definition.Base
import Kore.Pattern.Base

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
                [ ("con1", con1)
                , ("con2", con2)
                , ("con3", con3)
                , ("con4", con4)
                , ("f1", f1)
                , ("f2", f2)
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

var :: VarName -> Sort -> Term
var variableName variableSort = Var $ Variable{variableSort, variableName}

dv :: Sort -> Text -> Term
dv = DomainValue

app :: Symbol -> [Term] -> Term
app = SymbolApplication

asTotalFunction, asPartialFunction, asConstructor :: SymbolAttributes
asTotalFunction = SymbolAttributes TotalFunction False False
asPartialFunction = SymbolAttributes PartialFunction False False
asConstructor = SymbolAttributes Constructor False False

con1, con2, con3, con4, f1, f2 :: Symbol
con1 =
    Symbol
        { name = "con1"
        , resultSort = someSort
        , argSorts = [someSort]
        , attributes = asConstructor
        }
con2 =
    Symbol
        { name = "con2"
        , resultSort = someSort
        , argSorts = [someSort]
        , attributes = asConstructor
        }
con3 =
    Symbol
        { name = "con3"
        , resultSort = someSort
        , argSorts = [someSort, someSort]
        , attributes = asConstructor
        }
con4 =
    Symbol
        { name = "con4"
        , resultSort = aSubsort
        , argSorts = [someSort, someSort]
        , attributes = asConstructor
        }
f1 =
    Symbol
        { name = "f1"
        , resultSort = someSort
        , argSorts = [someSort]
        , attributes = asTotalFunction
        }
f2 =
    Symbol
        { name = "f2"
        , resultSort = someSort
        , argSorts = [someSort]
        , attributes = asPartialFunction
        }
