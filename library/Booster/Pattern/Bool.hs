{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2024
License     : BSD-3-Clause
-}
module Booster.Pattern.Bool (
    foldAndBool,
    isBottom,
    negateBool,
    splitBoolPredicates,
    -- patterns
    pattern TrueBool,
    pattern FalseBool,
    pattern NotBool,
    pattern EqualsInt,
    pattern NEqualsInt,
    pattern EqualsK,
    pattern NEqualsK,
    pattern InternalCeil,
    pattern SetIn,
) where

import Data.ByteString.Char8 (ByteString)

import Booster.Definition.Attributes.Base (
    SMTType (SMTHook),
    SymbolAttributes (SymbolAttributes),
    SymbolType (TotalFunction),
    pattern CanBeEvaluated,
    pattern CannotBeEvaluated,
    pattern IsNotAssoc,
    pattern IsNotIdem,
    pattern IsNotMacroOrAlias, 
 )
import Booster.Pattern.Base (
    Pattern,
    Predicate (..),
    Symbol (Symbol),
    Term,
    constraints,
    pattern DomainValue,
    pattern SortBool,
    pattern SortInt,
    pattern SortK,
    pattern SymbolApplication, 
    pattern SortKItem,
    pattern SortSet,
 )
import Booster.Pattern.Util (isConcrete, sortOfTerm)
import Booster.SMT.Base (SExpr (Atom), SMTId (..))

pattern TotalFunctionWithSMT :: ByteString -> SymbolAttributes
pattern TotalFunctionWithSMT hook =
    SymbolAttributes
        TotalFunction
        IsNotIdem
        IsNotAssoc
        IsNotMacroOrAlias
        CanBeEvaluated
        Nothing
        (Just (SMTHook (Atom (SMTId hook))))

pattern AndBool :: Term -> Term -> Term
pattern AndBool l r =
    SymbolApplication
        ( Symbol
                "Lbl'Unds'andBool'Unds'"
                []
                [SortBool, SortBool]
                SortBool
                (TotalFunctionWithSMT "and")
            )
        []
        [l, r]

pattern NotBool :: Term -> Term
pattern NotBool t =
    SymbolApplication
        ( Symbol
                "LblnotBool'Unds'"
                []
                [SortBool]
                SortBool
                (TotalFunctionWithSMT "not")
            )
        []
        [t]

pattern EqualsInt, NEqualsInt, EqualsK, NEqualsK :: Term -> Term -> Term
pattern EqualsInt a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsEqls'Int'Unds'"
                []
                [SortInt, SortInt]
                SortBool
                (TotalFunctionWithSMT "=")
            )
        []
        [a, b]
pattern NEqualsInt a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsSlshEqls'Int'Unds'"
                []
                [SortInt, SortInt]
                SortBool
                (TotalFunctionWithSMT "distinct")
            )
        []
        [a, b]
pattern EqualsK a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsEqls'K'Unds'"
                []
                [SortK, SortK]
                SortBool
                (TotalFunctionWithSMT "=")
            )
        []
        [a, b]
pattern SetIn a b =
    SymbolApplication
        ( Symbol
                "LblSet'Coln'in"
                []
                [SortKItem, SortSet]
                SortBool
                (SymbolAttributes
                    TotalFunction
                    IsNotIdem
                    IsNotAssoc
                    IsNotMacroOrAlias
                    CanBeEvaluated
                    Nothing
                    Nothing
                )
        )
        []
        [a, b]
pattern NEqualsK a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsSlshEqls'K'Unds'"
                []
                [SortK, SortK]
                SortBool
                (TotalFunctionWithSMT "distinct")
            )
        []
        [a, b]

pattern InternalCeil :: Term -> Term
pattern InternalCeil t <- SymbolApplication (Symbol "internal_ceil" _ _ _ _) [] [t]
    where
        InternalCeil t = 
            SymbolApplication
                ( Symbol
                        "internal_ceil"
                        []
                        [sortOfTerm t]
                        SortBool
                        ( SymbolAttributes
                            TotalFunction
                            IsNotIdem
                            IsNotAssoc
                            IsNotMacroOrAlias
                            CannotBeEvaluated
                            Nothing
                            Nothing)
                    )
                []
                [t]

pattern TrueBool, FalseBool :: Term
pattern TrueBool = DomainValue SortBool "true"
pattern FalseBool = DomainValue SortBool "false"

negateBool :: Term -> Term
negateBool TrueBool = FalseBool
negateBool FalseBool = TrueBool
negateBool x = NotBool x

foldAndBool :: [Term] -> Term
foldAndBool [] = TrueBool
foldAndBool [x] = x
foldAndBool (FalseBool : _) = FalseBool
foldAndBool (TrueBool : xs) = foldAndBool xs
foldAndBool (x : xs) = AndBool x $ foldAndBool xs

isBottom :: Pattern -> Bool
isBottom = (Predicate FalseBool `elem`) . constraints

{- | We want to break apart predicates of type `Y1 andBool ... Yn` apart, in case some of the `Y`s are abstract
which prevents the original expression from being fed to the LLVM simplifyBool function
-}
splitBoolPredicates :: Predicate -> [Predicate]
splitBoolPredicates p@(Predicate t)
    | isConcrete t = [p]
    | otherwise = case t of
        AndBool l r -> concatMap (splitBoolPredicates . Predicate) [l, r]
        other -> [Predicate other]
