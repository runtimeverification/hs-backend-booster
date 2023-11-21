{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.LowLevelCodec (
    readResponse,
    parseSExpr,
    encodeQuery,
    encodeDeclaration,
) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Ratio
import Numeric (readHex)
import Text.Read

import Booster.SMT.Base

readResponse :: BS.ByteString -> Response
readResponse =
    either (Error . BS.pack) id . A.parseOnly responseP

parseSExpr :: BS.ByteString -> Either String SExpr
parseSExpr = A.parseOnly sexpP

-- S-Expression and response parsing

responseP :: A.Parser Response
responseP =
    A.string "success" $> Success -- UNUSED?
        <|> A.string "sat" $> Sat
        <|> A.string "unsat" $> Unsat
        <|> A.string "unknown" $> Unknown
        <|> A.char '(' *> errOrValuesP <* A.char ')'

errOrValuesP :: A.Parser Response
errOrValuesP =
    A.string "error " *> (Error <$> stringP)
        <|> Values <$> A.many1' pairP

stringP :: A.Parser BS.ByteString
stringP = A.char '"' *> A.takeWhile1 (/= '"') <* A.char '"'

pairP :: A.Parser (SExpr, Value)
pairP = do
    A.skipSpace *> void (A.char '(')
    !s <- sexpP
    A.skipSpace
    !v <- valueP
    void (A.char ')')
    pure (s, v)

-- TODO could be parsed directly using attoparsec parsers
valueP :: A.Parser Value
valueP = fmap sexprToVal sexpP

sexprToVal :: SExpr -> Value
sexprToVal = \case
    Atom "true" ->
        Bool True
    Atom "false" ->
        Bool False
    Atom SmtId{bs}
        | ('#' : 'b' : ds) <- BS.unpack bs
        , Just n <- binLit ds ->
            Bits (length ds) n
        | ('#' : 'x' : ds) <- BS.unpack bs
        , [(n, [])] <- readHex ds ->
            Bits (4 * length ds) n
        | Just n <- readMaybe (BS.unpack bs) ->
            Int n
    List [Atom "-", x]
        | Int a <- sexprToVal x ->
            Int (negate a)
    List [Atom "/", x, y]
        | Int a <- sexprToVal x
        , Int b <- sexprToVal y ->
            Real (a % b)
    expr ->
        Other expr
  where
    binLit cs = do
        ds <- mapM binDigit cs
        return $ sum $ zipWith (*) (reverse ds) powers2
    powers2 = 1 : map (2 *) powers2
    binDigit '0' = Just 0
    binDigit '1' = Just 1
    binDigit _ = Nothing

sexpP :: A.Parser SExpr
sexpP = parseAtom <|> parseList
  where
    parseList = List <$> (A.char '(' *> sexpP `A.sepBy` whiteSpace <* A.char ')')

    parseAtom = Atom . SmtId <$> A.takeWhile1 (not . isSpecial)

    isSpecial c = isSpace c || c `elem` ['(', ')', ';']

    whiteSpace = A.takeWhile1 isSpace

-----------------------------------------------------
-- Encoding commands as SExprs and rendering a BS builder

toBuilder :: SExpr -> BS.Builder
toBuilder = \case
    Atom (SmtId bs) ->
        BS.byteString bs
    List [] ->
        inParens ""
    List (x : rest) ->
        inParens . mconcat $ toBuilder x : [BS.char8 ' ' <> toBuilder y | y <- rest]
  where
    inParens b = BS.char8 '(' <> b <> BS.char8 ')'

encodeQuery :: QueryCommand -> BS.Builder
encodeQuery = \case
    CheckSat -> BS.shortByteString "(check-sat)"
    GetValue xs -> toBuilder $ List (atom "get-values" : xs)

atom :: String -> SExpr
atom = Atom . SmtId . BS.pack

encodeDeclaration :: DeclareCommand -> BS.Builder
encodeDeclaration =
    toBuilder . \case
        Assert x -> List [atom "assert", x]
        DeclareConst name sort -> List [atom "declare-const", Atom name, sortExpr sort]
        -- DeclareData ddcls -> not required (yet)
        DeclareSort name arity -> List [atom "declare-sort", Atom name, atom (show arity)]
        DeclareFunc name sorts sort ->
            List [atom "declare-fun", Atom name, List (map sortExpr sorts), sortExpr sort]

sortExpr :: SmtSort -> SExpr
sortExpr = \case
    SimpleSmtSort name -> Atom name
    SmtSort name args -> List (Atom name : map sortExpr args)