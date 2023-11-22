{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
    getModelFor,
) where

import Control.Monad.Logger
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Util (sortOfTerm)
import Booster.SMT.Base as SMT
import Booster.SMT.Runner as SMT
import Booster.SMT.Translate as SMT

{- |
Implementation of get-model request

Queries an SMT solver (given by SMTContext but assumed uninitialised,
passing the definition) for whether the given predicates are
satisfiable.

Returns a satisfying substitution of free variables
in the predicates if so.

Returns either 'Unsat' or 'Unknown' otherwise, depending on whether
the solver could determine 'Unsat'.
-}
getModelFor ::
    MonadLoggerIO io =>
    SMT.SMTContext ->
    KoreDefinition ->
    [Predicate] ->
    io (Either SMT.Response (Map Variable Term))
getModelFor ctxt def ps
    | null ps = pure $ Right Map.empty
    | otherwise = runSMT ctxt $ do
        let (smtAsserts, transState) =
                SMT.runTranslator $ mapM (fmap Assert . SMT.translateTerm . getTerm) ps
            freeVars =
                Set.unions $ map ((.variables) . getAttributes . getTerm) ps

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
    getTerm (Predicate t) = t -- FIXME want a named field for this!
    getVar (Var v) = v
    getVar _ = error "not a var"
