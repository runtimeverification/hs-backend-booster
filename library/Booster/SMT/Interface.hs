{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
    SMTContext,
    initSolver,
    closeSolver,
    getModelFor,
) where

import Control.Monad.Logger qualified as Log
import Data.ByteString.Char8 qualified as BS
import Data.Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)

import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Util (sortOfTerm)
import Booster.SMT.Base as SMT
import Booster.SMT.Runner as SMT
import Booster.SMT.Translate as SMT

initSolver :: Log.MonadLoggerIO io => KoreDefinition -> Maybe FilePath -> io SMT.SMTContext
initSolver def mbTranscript = do
    ctxt <- mkContext mbTranscript
    logSMT "Checking definition prelude"
    check <-
        runSMT ctxt $
            mapM_ runCmd (smtDeclarations def) >> runCmd CheckSat
    case check of
        Sat -> pure ctxt
        other -> do
            logSMT $ "Initial SMT definition check returned " <> pack (show other)
            error "Refusing to work with a potentially inconsistent SMT setup"

closeSolver :: Log.MonadLoggerIO io => SMT.SMTContext -> io ()
closeSolver ctxt = do
    logSMT "Closing SMT solver"
    closeContext ctxt

{- |
Implementation of get-model request

Queries an SMT solver (given by SMTContext but assumed uninitialised,
passing the definition) for whether the given predicates and
the supplied substitution are satisfiable together.

Returns a satisfying substitution of free variables
in the predicates if so.

Returns either 'Unsat' or 'Unknown' otherwise, depending on whether
the solver could determine 'Unsat'.
-}
getModelFor ::
    Log.MonadLoggerIO io =>
    SMT.SMTContext ->
    [Predicate] ->
    Map Variable Term -> -- supplied substitution
    io (Either SMT.Response (Map Variable Term))
getModelFor ctxt ps subst
    | null ps && Map.null subst = do
        logSMT "No constraints or substitutions to check, returning Sat"
        pure $ Right Map.empty
    | otherwise = runSMT ctxt $ do
        logSMT $ "Checking, constraint count " <> pack (show $ Map.size subst + length ps)
        let (smtAsserts, transState) =
                SMT.runTranslator $ do
                    let mkSMTEquation v t =
                            SMT.eq <$> SMT.translateTerm (Var v) <*> SMT.translateTerm t
                    smtSubst <-
                        mapM (fmap Assert . uncurry mkSMTEquation) $ Map.assocs subst
                    smtPs <-
                        mapM (fmap Assert . SMT.translateTerm . coerce) ps
                    pure $ smtSubst <> smtPs
            freeVars =
                Set.unions $
                    Map.keysSet subst : map ((.variables) . getAttributes . coerce) ps

        let freeVarsMap =
                Map.filterWithKey (const . (`Set.member` Set.map Var freeVars)) transState.mappings
            freeVarsToSExprs = Map.mapKeys getVar $ Map.map Atom freeVarsMap

        runCmd_ SMT.Push -- assuming the prelude has been run already,

        -- declare-const all introduced variables (free in predicates
        -- as well as abstraction variables) before sending assertions
        mapM_
            runCmd
            [ DeclareConst smtId (SMT.smtSort $ sortOfTerm trm)
            | (trm, smtId) <- Map.assocs transState.mappings
            ]

        -- assert the given predicates
        mapM_ runCmd smtAsserts

        satResponse <- runCmd CheckSat

        case satResponse of
            Error msg -> do
                runCmd_ SMT.Pop
                error $ "SMT Error: " <> BS.unpack msg
            Unsat -> do
                runCmd_ SMT.Pop
                pure $ Left Unsat
            Unknown ->do
                runCmd_ SMT.Pop
                pure $ Left Unknown
            Values{} -> do
                runCmd_ SMT.Pop
                error $ "Unexpected SMT response " <> show satResponse
            Success -> do
                runCmd_ SMT.Pop
                error $ "Unexpected SMT response " <> show satResponse
            Sat -> do
                response <-
                    if Map.null freeVarsToSExprs
                        then pure $ Values []
                        else runCmd $ GetValue (Map.elems freeVarsToSExprs)
                runCmd_ SMT.Pop
                case response of
                    Error msg ->
                        error $ "SMT Error: " <> BS.unpack msg
                    Values pairs ->
                        let x :: Map Variable Term
                            x =
                                Map.map (valueToTerm transState) $
                                    Map.compose (Map.fromList pairs) freeVarsToSExprs
                         in pure $ Right x
                    other ->
                        error $ "Unexpected SMT response to GetValue: " <> show other
  where
    getVar (Var v) = v
    getVar _ = error "not a var"

logSMT :: Log.MonadLoggerIO io => Text -> io ()
logSMT = Log.logOtherNS "booster" (Log.LevelOther "SMT")
