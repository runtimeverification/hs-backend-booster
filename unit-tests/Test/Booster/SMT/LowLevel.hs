{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.SMT.LowLevel (
    test_smoke,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Char8 qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Booster.SMT.Base as SMT
import Booster.SMT.LowLevelCodec
import Booster.SMT.Runner

test_smoke :: TestTree
test_smoke =
    testGroup
        "Smoke test for SMT bindings (low-level)"
        [ emptyIsSat
        , declTests
        , codecTests
        ]

----------------------------------------
emptyIsSat :: TestTree
emptyIsSat =
    testCase "An empty solver responds to (check-sat) with sat" $
    (Sat @=?) =<< runSatAfter []

declTests :: TestTree
declTests =
    testGroup
        "Tests for declarations"
        [ testCase "declare-const" $ runsOK [Declare $ DeclareConst "x" smtInt]
        , testCase "declare-fun(_)" $ runsOK [Declare $ DeclareFunc "f" [smtInt] smtInt]
        , testCase "declare-fun(_, _)" $ runsOK [Declare $ DeclareFunc "g" [smtInt, smtInt] smtInt]
        , testCase "declare simple sort" $ runsOK [Declare $ DeclareSort "SomeSort" 0]
        , testCase "declare simple sort" $ runsOK [Declare $ DeclareSort "OtherSort" 1]
        , testCase "declare custom-sorted function" $
            let sortName = "CustomSort"
             in runsOK [ Declare $ DeclareSort sortName 1
                       , Declare $ DeclareFunc "f" [SmtSort sortName [smtInt]] smtInt
                       ]
        -- , runsOK [Declare $ Assert $ ]
        ]
  where
    -- FIXME move this to the library modules!
    smtInt = SimpleSmtSort "Int"

runSatAfter :: [SmtCommand] -> IO SMT.Response
runSatAfter commands = runNoLoggingT $ do
    ctxt <- liftIO $ mkContext Nothing
    result <- runSMT ctxt $ mapM_ runCmd commands >> runCmd CheckSat
    liftIO $ closeContext ctxt
    pure result

runsOK :: [SmtCommand] -> Assertion
runsOK cmds = runSatAfter cmds >>= (Sat @=?)

----------------------------------------
codecTests :: TestTree
codecTests =
    testGroup
        "Codec tests"
        [ responseParsing
        , valueParsing
        ]

responseParsing :: TestTree
responseParsing =
    testGroup
        "Response parsing"
        [ "sat" `parsesTo` Sat
        , "unsat" `parsesTo` Unsat
        , "unknown" `parsesTo` Unknown
        , "success" `parsesTo` Success
        , "(error \"Something was wrong\")" `parsesTo` Error "Something was wrong"
        , "((x 0))" `parsesTo` Values [(Atom "x", SMT.Int 0)]
        , "((x 0) (y true))" `parsesTo` Values [(Atom "x", SMT.Int 0), (Atom "y", SMT.Bool True)]
        ]
  where
    parsesTo :: BS.ByteString -> SMT.Response -> TestTree
    input `parsesTo` expected =
        testCase (show input <> " parses to " <> show expected) $
            expected @=? readResponse input

valueParsing :: TestTree
valueParsing = error "implement me"
