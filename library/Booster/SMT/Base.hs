{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Base (
    module Booster.SMT.Base,
) where

import Data.ByteString.Char8 qualified as BS
import Data.String

{- SMT lib 2 commands and responses

  Follows smtlib2 v2.6 grammar, see
  https://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2021-05-12.pdf

  Commands are grouped into declarations (carry data, no response),
  queries (carry no data, response is important), and session control.
-}

newtype SmtId = SmtId {bs :: BS.ByteString}
    deriving stock (Eq, Ord, Show)
    deriving newtype (IsString)

data SmtSort
    = SimpleSmtSort SmtId
    | SmtSort SmtId [SmtSort]
    deriving stock (Eq, Ord, Show)

data SExpr -- SmtTerm
    = Atom SmtId
    | List [SExpr]
    deriving stock (Eq, Ord, Show)

data SmtCommand
    = Declare DeclareCommand -- no response required
    | Query QueryCommand -- response essential
    | Control ControlCommand
    deriving stock (Eq, Ord, Show)

data DeclareCommand
    = Assert SExpr
    | DeclareConst SmtId SmtSort
    | DeclareSort SmtId Int
    | DeclareFunc SmtId [SmtSort] SmtSort
    --    | DeclareData [DataDecl] NOT NECESSARY?
    deriving stock (Eq, Ord, Show)

type DataDecl = String -- HACK

data ControlCommand
    = Push -- Int
    | Pop -- Int
    | Exit
    deriving stock (Eq, Ord, Show)

data QueryCommand
    = CheckSat
    | GetValue [SExpr] -- for get-model
    deriving stock (Eq, Ord, Show)

data Response
    = Success -- for command_
    | Sat
    | Unsat
    | Unknown
    | Values [(SExpr, Value)]
    | Error BS.ByteString
    deriving stock (Eq, Ord, Show)

-- Common values returned by SMT solvers.
data Value
    = Bool !Bool
    | Int !Integer
    | Real !Rational
    | Bits !Int !Integer
    | Other !SExpr
    deriving stock (Eq, Ord, Show)

----------------------------------------
-- well-known arithmetic functions, implemented through Num

instance Num SExpr where
    a + b = List [Atom "+", a, b]
    a - b = List [Atom "-", a, b]
    a * b = List [Atom "*", a, b]
    negate a = List [Atom "-", a]
    abs a = List [Atom "abs", a]
    signum _ = error "signum @SExpr not implemented"
    fromInteger n
        | n >= 0 = Atom . SmtId . BS.pack $ show n
        | otherwise = List [Atom "-", fromInteger (negate n)]

smtInt, smtBool :: SmtSort
smtInt = SimpleSmtSort "Int"
smtBool = SimpleSmtSort "Bool"

-- well-known combinators with boolean result
eq, neq, le, leq, gr, geq :: SExpr -> SExpr -> SExpr
eq = mkOp "="
neq = mkOp "distinct"
le = mkOp "<"
leq = mkOp "<="
gr = mkOp ">"
geq = mkOp ">="

mkOp :: BS.ByteString -> SExpr -> SExpr -> SExpr
mkOp opString a b = List [Atom $ SmtId opString, a, b]

smtnot :: SExpr -> SExpr
smtnot x = List [Atom "not", x]
