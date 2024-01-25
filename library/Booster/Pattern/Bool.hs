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
    splitAndBools,
    isDefinitionalEquality,
    deriveDefinitonalEqualities,
    -- patterns
    pattern TrueBool,
    pattern FalseBool,
    pattern NotBool,
    pattern AndBool,
    pattern EqualsInt,
    pattern EqualsBool,
    pattern NEqualsInt,
    pattern EqualsK,
    pattern NEqualsK,
    pattern SetIn,
) where

import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Coerce
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Debug.Trace qualified as Debug

import Booster.Definition.Attributes.Base (
    SMTType (SMTHook),
    SymbolAttributes (SymbolAttributes),
    SymbolType (TotalFunction),
    pattern CanBeEvaluated,
    pattern IsNotAssoc,
    pattern IsNotIdem,
    pattern IsNotMacroOrAlias,
 )
import Booster.Pattern.Base (
    Pattern,
    Predicate (..),
    Symbol (Symbol),
    Term,
    Variable,
    constraints,
    pattern DomainValue,
    pattern KSeq,
    pattern SortBool,
    pattern SortInt,
    pattern SortK,
    pattern SortKItem,
    pattern SortSet,
    pattern SymbolApplication,
    pattern Var,
 )
import Booster.Pattern.Util (isConcrete, isRuleVar, substituteInPredicate)
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
        Nothing

pattern HookedFunctionWithSMT :: Text -> ByteString -> SymbolAttributes
pattern HookedFunctionWithSMT hook smt =
    SymbolAttributes
        TotalFunction
        IsNotIdem
        IsNotAssoc
        IsNotMacroOrAlias
        CanBeEvaluated
        Nothing
        (Just (SMTHook (Atom (SMTId smt))))
        (Just hook)

pattern AndBool :: Term -> Term -> Term
pattern AndBool l r =
    SymbolApplication
        ( Symbol
                "Lbl'Unds'andBool'Unds'"
                []
                [SortBool, SortBool]
                SortBool
                (HookedFunctionWithSMT "BOOL.and" "and")
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
                (HookedFunctionWithSMT "BOOL.not" "not")
            )
        []
        [t]

pattern EqualsInt, EqualsBool, NEqualsInt, EqualsK, NEqualsK, SetIn :: Term -> Term -> Term
pattern EqualsInt a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsEqls'Int'Unds'"
                []
                [SortInt, SortInt]
                SortBool
                (HookedFunctionWithSMT "INT.eq" "=")
            )
        []
        [a, b]
pattern EqualsBool a b =
    SymbolApplication
        ( Symbol
                "Lbl'UndsEqlsEqls'Bool'Unds'"
                []
                [SortBool, SortBool]
                SortBool
                (HookedFunctionWithSMT "BOOL.eq" "=")
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
                (HookedFunctionWithSMT "BOOL.neq" "distinct")
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
                (HookedFunctionWithSMT "KEQUAL.eq" "=")
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
                ( SymbolAttributes
                        TotalFunction
                        IsNotIdem
                        IsNotAssoc
                        IsNotMacroOrAlias
                        CanBeEvaluated
                        Nothing
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
                (HookedFunctionWithSMT "KEQUAL.ne" "distinct")
            )
        []
        [a, b]

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
        _other -> [p]

{- | Break apart a predicate composed of top-level Y1 andBool ... Yn
(not considering whether any of the subterms is concrete).
-}
splitAndBools :: Predicate -> [Predicate]
splitAndBools p@(Predicate t)
    | AndBool l r <- t = concatMap (splitAndBools . Predicate) [l, r]
    | otherwise = [p]

{- | A predicate is a definitional equality if and only if:
     * it has an equality symbol at the top level
     * it has a Rule# variable on the lhs
-}
isDefinitionalEquality :: Predicate -> Bool
isDefinitionalEquality (Predicate term) =
    case term of
        -- EqualsK (KSeq _ (Var v)) _rhs -> isRuleVar v
        -- EqualsInt (Var v) _rhs -> isRuleVar v
        -- EqualsBool (Var v) _rhs -> isRuleVar v
        EqualsK (KSeq _ (Var v)) _rhs -> True
        EqualsInt (Var v) _rhs -> True
        EqualsBool (Var v) _rhs -> True
        _ -> False

mkSubstitutionItem :: Predicate -> Maybe (Variable, Term)
mkSubstitutionItem (Predicate term) =
    case term of
        EqualsK (KSeq _ (Var v)) (KSeq _ rhs) -> Just (v, rhs)
        EqualsInt (Var v) rhs -> Just (v, rhs)
        EqualsBool (Var v) rhs -> Just (v, rhs)
        _ -> Nothing

{- | de-duplicate definitional equalities by only taking first assignments.
  partition a list of predicates into definitional equalities and other predicates.
  Once definitional equalities are derived, they are applied as a substitution to the
  remaining predicates.
  If a variable is assigned twice, only the first assignment is considered a definitional equality,
  and all conservative occurrences of that variable will be substituted with the term
  from the first assignment
-}

-- deriveDefinitonalEqualities :: [Predicate] -> (Map Variable Term, [Predicate])
deriveDefinitonalEqualities :: [Predicate] -> ([Predicate], [Predicate])
deriveDefinitonalEqualities ps =
    let (candidates, nonEqualities) = List.partition isDefinitionalEquality ps
        substitutionItems = mapMaybe mkSubstitutionItem candidates
        uniqueLhsVars = Set.fromList . map fst $ substitutionItems
        (firstAssignments, otherAssignments) = List.partition ((`Set.member` uniqueLhsVars) . fst) substitutionItems
        definitonalEqualities = Map.fromList firstAssignments
        subsitutedOtherAssignments =
            map (substituteInPredicate definitonalEqualities . coerce . (\(x, y) -> EqualsK (Var x) (coerce y))) otherAssignments
        substitutedNonEqualities = map (substituteInPredicate definitonalEqualities) nonEqualities
        defEqsAsPredicates = map (coerce . (\(x, y) -> EqualsK (Var x) (coerce y))) (Map.toList definitonalEqualities)
     in (defEqsAsPredicates, substitutedNonEqualities <> subsitutedOtherAssignments)
