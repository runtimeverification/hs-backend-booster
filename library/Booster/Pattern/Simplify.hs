{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Simplify (
    simplifyPredicate,
    splitBoolPredicates,
    simplifyConcrete,
) where

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe, isJust)

import Booster.Definition.Base
import Booster.LLVM (simplifyBool, simplifyTerm)
import Booster.LLVM.Internal qualified as LLVM
import Booster.Pattern.Base
import Booster.Pattern.Util (isConcrete, sortOfTerm)

{- | We want to break apart predicates of type `X #Equals Y1 andBool ... Yn` into
`X #Equals Y1, ..., X #Equals  Yn` in the case when some of the `Y`s are abstract
and thus the whole original expression could not be fed to the LLVM simplifyBool function
-}
splitBoolPredicates :: Predicate -> [Predicate]
splitBoolPredicates = \case
    p@(EqualsTerm l r) | isConcrete l && isConcrete r -> [p]
    EqualsTerm (AndBool ls) r -> concatMap (splitBoolPredicates . flip EqualsTerm r) ls
    EqualsTerm l (AndBool rs) -> concatMap (splitBoolPredicates . EqualsTerm l) rs
    other -> [other]

simplifyPredicate :: Maybe LLVM.API -> Predicate -> Predicate
simplifyPredicate mApi = \case
    AndPredicate l r -> case (simplifyPredicate mApi l, simplifyPredicate mApi r) of
        (Bottom, _) -> Bottom
        (_, Bottom) -> Bottom
        (Top, r') -> r'
        (l', Top) -> l'
        (l', r') -> AndPredicate l' r'
    Bottom -> Bottom
    p@(Ceil _) -> p
    p@(EqualsTerm l r) ->
        fromMaybe p $ do
            api <- mApi
            guard (sortOfTerm l == SortBool)
            let adjustTerm t = do
                    if (isConcrete t)
                        then pure t
                        else do
                            tNew <- evalSizeWordStack t
                            guard (isConcrete tNew)
                            pure tNew
            l' <- adjustTerm l
            r' <- adjustTerm r
            pure $ if simplifyBool api l' == simplifyBool api r'
                       then Top
                       else Bottom
    EqualsPredicate l r -> EqualsPredicate (simplifyPredicate mApi l) (simplifyPredicate mApi r)
    p@(Exists _ _) -> p
    p@(Forall _ _) -> p
    Iff l r -> Iff (simplifyPredicate mApi l) (simplifyPredicate mApi r)
    Implies l r -> Implies (simplifyPredicate mApi l) (simplifyPredicate mApi r)
    p@(In _ _) -> p
    Not p -> case simplifyPredicate mApi p of
        Top -> Bottom
        Bottom -> Top
        p' -> p'
    Or l r -> Or (simplifyPredicate mApi l) (simplifyPredicate mApi r)
    Top -> Top

{- | traverses a term top-down, using a given LLVM dy.lib to simplify
 the concrete parts (leaving variables alone)
-}
simplifyConcrete :: Maybe LLVM.API -> KoreDefinition -> Term -> Term
simplifyConcrete Nothing _ trm = trm
simplifyConcrete (Just mApi) def trm = recurse trm
  where
    recurse :: Term -> Term
    -- recursion scheme for this?
    --     cata $ \case does not work here, would need helpers for TermF not Term
    --         t | isConcreteF t -> simplifyTerm dl def t (sortOfTerm t)
    --         other -> embed other\
    recurse t@(Term attributes _)
        | attributes.isEvaluated =
            t
        | isConcrete t =
            simplifyTerm mApi def t (sortOfTerm t)
        | otherwise =
            case t of
                var@Var{} ->
                    var -- nothing to do. Should have isEvaluated set
                dv@DomainValue{} ->
                    dv -- nothing to do. Should have isEvaluated set
                AndTerm t1 t2 ->
                    AndTerm (recurse t1) (recurse t2)
                SymbolApplication sym sorts args ->
                    SymbolApplication sym sorts (map recurse args)
                Injection sources target sub ->
                    Injection sources target $ recurse sub

----------------------------------------
-- gross hack: implement #sizeWordStack evaluation for use in predicates
--
-- If a term's function symbol is '#sizeWordStack' with a single
-- argument, traverse the spine of the cons (_:_) chain (until
-- .WordStack is found). Return 'Nothing' if unexpected structure is
-- found.
evalSizeWordStack :: Term -> Maybe Term
evalSizeWordStack t@(Term attributes _)
    | attributes.isEvaluated = -- Var, DomainValue
        Nothing
    | SymbolApplication sym _sorts [ws] <- t
    , sym.name == sizeWordStackName =
        fmap (DomainValue intSort . BS.pack . show) $ countSpine 0 ws
    | SymbolApplication sym sorts args <- t =
        let evaluatedArgs :: [Maybe Term]
            evaluatedArgs = map evalSizeWordStack args
            changesMade = any isJust evaluatedArgs
            newArgs = zipWith fromMaybe args evaluatedArgs
         in if changesMade then Just (SymbolApplication sym sorts newArgs) else Nothing
    | Injection source target sub <- t =
          fmap (Injection source target) $ evalSizeWordStack sub
    | AndTerm _t1 _t2 <- t = -- this is not going to appear in predicates anyway
          Nothing
    | otherwise = Nothing
  where
    sizeWordStackName = "Lbl'Hash'sizeWordStack'LParUndsRParUnds'EVM-TYPES'Unds'Int'Unds'WordStack"
    intSort = SortApp "SortInt" []
    consWordStackName = "Lbl'UndsColnUndsUnds'EVM-TYPES'Unds'WordStack'Unds'Int'Unds'WordStack"
    nilWordStackName = "Lbl'Stop'WordStack'Unds'EVM-TYPES'Unds'WordStack"

    countSpine :: Int -> Term -> Maybe Int
    countSpine n = \case
        SymbolApplication sym _ []
            | sym.name == nilWordStackName -> Just n
        SymbolApplication sym _ [_hd, tl]
            | sym.name == consWordStackName -> countSpine (n + 1) tl
        _other -> Nothing
