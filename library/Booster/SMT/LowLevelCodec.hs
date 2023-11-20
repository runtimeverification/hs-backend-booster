{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}

module Booster.SMT.LowLevelCodec (
    readResponse,
    parseSExpr,
) where

import Control.Monad
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Ratio
import Numeric (readHex)
import Text.Read

import Booster.SMT.Base

readResponse :: BS.Builder -> Response
readResponse =
    either (Error . BS.pack) id
        . A.parseOnly responseP
        . BS.toStrict
        . BS.toLazyByteString

parseSExpr :: BS.Builder -> Either String SExpr
parseSExpr = A.parseOnly sexpP . BS.toStrict . BS.toLazyByteString

-- S-Expression and response parsing

responseP :: A.Parser Response
responseP = A.string "success" $> Success -- UNUSED?
        <|> A.string "sat" $>  Sat
        <|> A.string "unsat" $> Unsat
        <|> A.string "unknown" $> Unknown
        <|> A.char '(' *> errOrValuesP <* A.char ')'

errOrValuesP :: A.Parser Response
errOrValuesP = A.string "error " *> (Error <$> stringP)
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
        | ('#' : 'x' : ds) <- BS.unpack  bs
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
    parseList = List <$> (A.char '(' *> A.many' sexpP <* A.char ')')

    parseAtom = Atom . SmtId <$> A.takeWhile1 (not . isSpecial)

    isSpecial c = isSpace c || c `elem` ['(', ')', ';']
