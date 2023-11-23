{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
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
    KoreDefinition ->
    [Predicate] ->
    Map Variable Term -> -- supplied substitution
    io (Either SMT.Response (Map Variable Term))
getModelFor ctxt def ps subst
    | null ps && Map.null subst = do
        logSMT "No constraints or substitutions to check, returning Sat"
        pure $ Right Map.empty
    | otherwise = runSMT ctxt $ do
        logSMT $ "Checking, constraint count " <> pack (show $ Map.size subst + length ps)
        let (smtAsserts, transState) =
                SMT.runTranslator $ do
                    let mkSmtEquation v t =
                            SMT.eq <$> SMT.translateTerm (Var v) <*> SMT.translateTerm t
                    smtSubst <-
                        mapM (fmap Assert . uncurry mkSmtEquation) $ Map.assocs subst
                    smtPs <-
                        mapM (fmap Assert . SMT.translateTerm . coerce) ps
                    pure $ smtSubst <> smtPs
            freeVars =
                Set.unions $
                    Map.keysSet subst : map ((.variables) . getAttributes . coerce) ps

        let freeVarsMap =
                Map.filterWithKey (const . (`Set.member` Set.map Var freeVars)) transState.mappings
            freeVarsToSExprs = Map.mapKeys getVar $ Map.map Atom freeVarsMap

        -- runCmd SMT.Push     -- assuming the prelude has been run already,
        mapM_ runCmd $ smtDeclarations def -- if prelude has not been run

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
            Error msg ->
                error $ "SMT Error: " <> BS.unpack msg
            Unsat ->
                pure $ Left Unsat
            Unknown ->
                pure $ Left Unknown
            Values{} ->
                error $ "Unexpected SMT response " <> show satResponse
            Success ->
                error $ "Unexpected SMT response " <> show satResponse
            Sat -> do
                response <- runCmd $ GetValue (Map.elems freeVarsToSExprs)
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
