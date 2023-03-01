{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Simplify (
    simplifyPredicate,
    splitBoolPredicates,
    simplifyTerm,
) where

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe, isJust)

import Booster.Definition.Base
import Booster.LLVM qualified as LLVM
import Booster.LLVM.Internal qualified as LLVM (API)
import Booster.Pattern.Base
import Booster.Pattern.Util (isConcrete, sortOfTerm)

{- | We want to break apart predicates of type `X #Equals Y1 andBool ... Yn` into
`X #Equals Y1, ..., X #Equals  Yn` in the case when some of the `Y`s are abstract
and thus the whole original expression could not be fed to the LLVM simplifyBool function
-}
splitBoolPredicates :: Predicate -> [Predicate]
splitBoolPredicates = \case
    p@(EqualsTerm l r) | isConcrete l && isConcrete r -> [p]
    EqualsTerm (AndBool l1 l2) r -> concatMap (splitBoolPredicates . flip EqualsTerm r) [l1, l2]
    EqualsTerm l (AndBool r1 r2) -> concatMap (splitBoolPredicates . EqualsTerm l) [r1, r2]
    other -> [other]








simplifyPredicate :: Maybe LLVM.API ->  KoreDefinition -> Predicate -> Predicate
simplifyPredicate mApi def = \case
    AndPredicate l r -> case (simplifyPredicate mApi def l, simplifyPredicate mApi def r) of
        (Bottom, _) -> Bottom
        (_, Bottom) -> Bottom
        (Top, r') -> r'
        (l', Top) -> l'
        (l', r') -> AndPredicate l' r'
    Bottom -> Bottom
    p@(Ceil _) -> p
    p@(EqualsTerm l r)
        | sortOfTerm l == SortBool ->
            let adjustTerm t = fromMaybe t $ do
                    if isConcrete t
                        then pure t
                        else evalSizeWordStack t
                l' = adjustTerm l
                r' = adjustTerm r
            in
            case (simplifyTerm mApi def l', simplifyTerm mApi def r') of
                (TrueBool, TrueBool) -> Top
                (FalseBool, FalseBool) -> Top
                (TrueBool, FalseBool) -> Bottom
                (FalseBool, TrueBool) -> Bottom
                (l'', r'') -> EqualsTerm l'' r''
        | otherwise -> p
    EqualsPredicate l r -> EqualsPredicate (simplifyPredicate mApi def l) (simplifyPredicate mApi def r)
    p@(Exists _ _) -> p
    p@(Forall _ _) -> p
    Iff l r -> Iff (simplifyPredicate mApi def l) (simplifyPredicate mApi def r)
    Implies l r -> Implies (simplifyPredicate mApi def l) (simplifyPredicate mApi def r)
    p@(In _ _) -> p
    Not p -> case simplifyPredicate mApi def p of
        Top -> Bottom
        Bottom -> Top
        p' -> p'
    Or l r -> Or (simplifyPredicate mApi def l) (simplifyPredicate mApi def r)
    Top -> Top

{- | traverses a term top-down, using a given LLVM dy.lib to simplify
 the concrete parts (leaving variables alone)
-}
simplifyTerm :: Maybe LLVM.API -> KoreDefinition -> Term -> Term
simplifyTerm Nothing _ trm = trm
simplifyTerm (Just mApi) def trm = recurse trm
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
            LLVM.simplifyTerm mApi def t (sortOfTerm t)
        | otherwise =
            case t of
                var@Var{} ->
                    var -- nothing to do. Should have isEvaluated set
                dv@DomainValue{} ->
                    dv -- nothing to do. Should have isEvaluated set
                AndTerm t1 t2 ->
                    AndTerm (recurse t1) (recurse t2)

                AndBool l r -> case (recurse l, recurse r) of
                    (TrueBool, r') -> r'
                    (l', TrueBool) -> l'
                    (FalseBool, _) -> FalseBool
                    (_, FalseBool) -> FalseBool
                    (l', r') -> AndBool l' r'
                OrBool l r -> case (recurse l, recurse r) of
                    (FalseBool, r') -> r'
                    (l', FalseBool) -> l'
                    (TrueBool, _) -> TrueBool
                    (_, TrueBool) -> TrueBool
                    (l', r') -> OrBool l' r'
                NotBool a -> case recurse a of
                    FalseBool -> TrueBool
                    TrueBool -> FalseBool
                    a' -> NotBool a'
                LTEqInt l r -> case (recurse l, recurse r) of
                    (_, InfGas _) -> TrueBool
                    (InfGas _, _) -> FalseBool
                    (l', r') | isConcrete l' && isConcrete r' -> recurse $ LTEqInt l' r'
                             | otherwise -> LTEqInt l' r'
                LTInt l r -> case (recurse l, recurse r) of
                    (InfGas _, InfGas _) -> FalseBool
                    (_, InfGas _) -> TrueBool
                    (InfGas _, _) -> FalseBool
                    (l', r') | isConcrete l' && isConcrete r' -> recurse $ LTInt l' r'
                             | otherwise -> LTInt l' r'
                GTEqInt l r -> case (recurse l, recurse r) of
                    (InfGas _, _) -> TrueBool
                    (_, InfGas _) -> FalseBool
                    (l', r') | isConcrete l' && isConcrete r' -> recurse $ GTEqInt l' r'
                             | otherwise -> GTEqInt l' r'
                GTInt l r -> case (recurse l, recurse r) of
                    (InfGas _, InfGas _) -> FalseBool
                    (InfGas _, _) -> TrueBool
                    (_, InfGas _) -> FalseBool
                    (l', r') | isConcrete l' && isConcrete r' -> recurse $ GTInt l' r'
                             | otherwise -> GTInt l' r'

                PlusInt a b -> case (recurse a, recurse b) of
                    (InfGas a', InfGas b') -> InfGas $ PlusInt a' b'
                    (InfGas a', b') -> InfGas $ PlusInt a' b'
                    (a', InfGas b') -> InfGas $ PlusInt a' b'
                    (a', b') | isConcrete a' && isConcrete b' -> recurse $ PlusInt a' b'
                             | otherwise -> PlusInt a' b'

                MinusInt a b -> case (recurse a, recurse b) of
                    (InfGas a', InfGas b') -> InfGas $ MinusInt a' b'
                    (InfGas a', b') -> InfGas $ MinusInt a' b'
                    (a', b') | isConcrete a' && isConcrete b' -> recurse $ MinusInt a' b'
                             | otherwise -> MinusInt a' b'
                
                TimesInt a b -> case (recurse a, recurse b) of
                    (InfGas a', InfGas b') -> InfGas $ TimesInt a' b'
                    (InfGas a', b') -> InfGas $ TimesInt a' b'
                    (a', InfGas b') -> InfGas $ TimesInt a' b'
                    (a', b') | isConcrete a' && isConcrete b' -> recurse $ TimesInt a' b'
                             | otherwise -> TimesInt a' b'
                
                DivInt a b -> case (recurse a, recurse b) of
                    (InfGas a', b') -> InfGas $ DivInt a' b'
                    (_, InfGas _) -> DomainValue SortInt "0"
                    (a', b') | isConcrete a' && isConcrete b' -> recurse $ DivInt a' b'
                             | otherwise -> DivInt a' b'
                
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
