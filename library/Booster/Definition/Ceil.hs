module Booster.Definition.Ceil (
    module Booster.Definition.Ceil,
) where

import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.ApplyEquations
import Booster.Definition.Attributes.Base
import Booster.Pattern.Bool
import Control.Monad.Logger (MonadLoggerIO)
import Booster.Syntax.Json.Internalise (explodeAnd)
import Control.Monad (foldM, forM, when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Extra (concatMapM)
import Data.Coerce (coerce)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import qualified Prettyprinter.Render.String as Pretty
import Booster.Prettyprinter
import Prettyprinter
import Data.Maybe (fromMaybe)

computeCeilsDefinition :: MonadLoggerIO io => KoreDefinition -> io KoreDefinition
computeCeilsDefinition def@KoreDefinition{rewriteTheory} = do
  rewriteTheory' <-
    Map.fromList <$> forM (Map.toList rewriteTheory) ( \(k, v) ->
      (k,) . Map.fromList <$> (forM (Map.toList v) $ \(k', rs) ->
        (k',) <$> forM rs (computeCeilRule def)))
  pure def{rewriteTheory = rewriteTheory'}



computeCeilRule :: MonadLoggerIO io => KoreDefinition -> RewriteRule "Rewrite" -> io (RewriteRule "Rewrite")
computeCeilRule def r@RewriteRule{lhs, requires, rhs, computedAttributes}
  | null computedAttributes.notPreservesDefinednessReasons = pure r
  | otherwise = do
    (res, _, _) <- runEquationT False def Nothing mempty $ do
      !lhsCeils <- Set.fromList <$> computeCeil lhs
      !requiresCeils <- Set.fromList <$> concatMapM (computeCeil . coerce) (Set.toList requires)
      !rhsCeils <- Set.fromList <$>  computeCeil rhs

      liftIO $ do
        putStrLn "\n\n----------------------------\n"
        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ 
          pretty lhs

        putStrLn  "  =>"
        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ 
          pretty rhs 
        putStrLn  "  requires"
        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ vsep (map pretty $ Set.toList requires) 

        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "partial symbols found:" <+> vsep (map pretty computedAttributes.notPreservesDefinednessReasons)
        -- putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "LHS ceils:" <+> vsep (map pretty $ Set.toList lhsCeils)
        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "requiresCeils ceils:" <+> vsep (map pretty $ Set.toList $ requiresCeils Set.\\ lhsCeils)
        putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "RHS ceils:" <+> vsep (map pretty $ Set.toList $ rhsCeils Set.\\ lhsCeils)

      pure r
    case res of
      Left err -> do
        liftIO $ print err
        pure r
      Right r' -> pure r'


computeCeil :: MonadLoggerIO io => Term -> EquationT io [Term]
computeCeil term@(SymbolApplication symbol _ args)
  | symbol.attributes.symbolType /= PartialFunction = concatMapM computeCeil args
  | otherwise = do
      argCeils <- concatMapM computeCeil args
      ceils <- (.definition.ceils) <$> getConfig
      simplified <- applyEquations True ceils handleSimplificationEquation $ InternalCeil term
      -- liftIO $ putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "original ceil:" <+> pretty (InternalCeil term)
      -- when (simplified /= (InternalCeil term)) $ liftIO $ putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "applied ceil:" <+> pretty simplified
      if simplified == (InternalCeil term)
        then pure [InternalCeil term]
        else foldM processCeil argCeils $ splitBoolPredicates (Predicate simplified)
  where
    processCeil xs (Predicate (InternalCeil t)) = (<>xs) <$> computeCeil t
    processCeil xs (Predicate p) = pure $ p :xs

computeCeil (AndTerm l r) = concatMapM computeCeil [l,r]
computeCeil (Injection _ _ t) = computeCeil t
-- computeCeil (KMap def keyVals rest) = computeCeil $ externaliseKmapUnsafe def keyVals rest
-- computeCeil (KList def heads rest) = computeCeil $ externaliseKList def heads rest
-- computeCeil (KSet def elems rest) = computeCeil $ externaliseKSet def elems rest
computeCeil (KMap _ keyVals rest) = do
  recArgs <- concatMapM computeCeil $ concat [[k,v] | (k, v) <- keyVals] <> maybe [] (:[]) rest
  pure $ [NEqualsK a b | a <- map fst keyVals, b <- map fst keyVals, a /= b] -- missing [NotBool (InKeys a rest') | a <- map fst keyVals, rest' <- maybe [] (:[]) rest]
    <> recArgs
computeCeil (KList _ heads rest) = concatMapM computeCeil $ heads <> maybe [] (uncurry (:)) rest
computeCeil (KSet _ elems rest) = do
  recArgs <- concatMapM computeCeil $ elems <> maybe [] (:[]) rest
  -- forall a b in elems. a /= b and a \not\in rest and b \not\in rest
  pure $ [NEqualsK a b | a <- elems, b <- elems, a /= b] <> [NotBool (SetIn a rest') | a <- elems, rest' <- maybe [] (:[]) rest] <> recArgs
computeCeil DomainValue{} = pure []
computeCeil v@Var{} = pure [InternalCeil v]