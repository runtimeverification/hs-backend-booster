{
{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Internal Parser module for parsing KORE Text.
-}

module Kore.Syntax.Parser.Parser (
) where

import Data.ByteString.Lazy.Char8 qualified as B
import Data.Char qualified as Char
import Data.List (
    foldl',
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Numeric.Natural

import Kore.Syntax.Parser.Lexer
import Kore.Syntax.Parser.LexerWrapper
import Kore.Syntax.ParsedKore.Base
import Kore.Syntax.Json.Base as Json

}

%name aliasHeadStart AliasHead
%name attributesStart Attributes
%name definitionStart Definition
%name elementVariableStart ElementVariable
%name idStart Id
%name moduleStart Module
%name patternStart Pattern
%name sentenceStart Sentence
%name setVariableStart SetVariable
%name sortStart Sort
%name sortsParenStart SortsParen
%name sortVariableStart SortVariable
%name sortVariablesStart SortVariables
%name stringLiteralStart StringLiteral
%name symbolHeadStart SymbolHead

%tokentype { Token }
%monad { Alex }
%lexer { alexMonadScan >>= } { Token _ TokenEOF }
%error { happyError }

%token
    module               { Token _ TokenModule }
    endmodule            { Token _ TokenEndModule }
    import               { Token _ TokenImport }
    sort                 { Token _ TokenSort }
    symbol               { Token _ TokenSymbol }
    where                { Token _ TokenWhere }
    alias                { Token _ TokenAlias }
    axiom                { Token _ TokenAxiom }
    claim                { Token _ TokenClaim }
    hookedSort           { Token _ TokenHookedSort }
    hookedSymbol         { Token _ TokenHookedSymbol }
    ':='                 { Token _ TokenColonEqual }
    ':'                  { Token _ TokenColon }
    '{'                  { Token _ TokenLeftBrace }
    '}'                  { Token _ TokenRightBrace }
    '['                  { Token _ TokenLeftBracket }
    ']'                  { Token _ TokenRightBracket }
    '('                  { Token _ TokenLeftParen }
    ')'                  { Token _ TokenRightParen }
    ','                  { Token _ TokenComma }
    top                  { Token _ TokenTop }
    bottom               { Token _ TokenBottom }
    not                  { Token _ TokenNot }
    and                  { Token _ TokenAnd }
    or                   { Token _ TokenOr }
    implies              { Token _ TokenImplies }
    iff                  { Token _ TokenIff }
    exists               { Token _ TokenExists }
    forall               { Token _ TokenForall }
    mu                   { Token _ TokenMu }
    nu                   { Token _ TokenNu }
    ceil                 { Token _ TokenCeil }
    floor                { Token _ TokenFloor }
    equals               { Token _ TokenEquals }
    in                   { Token _ TokenIn }
    next                 { Token _ TokenNext }
    rewrites             { Token _ TokenRewrites }
    dv                   { Token _ TokenDomainValue }
    leftAssoc            { Token _ TokenLeftAssoc }
    rightAssoc           { Token _ TokenRightAssoc }
    ident                { Token _ (TokenIdent _) }
    setIdent             { Token _ (TokenSetIdent _) }
    string               { Token _ (TokenString $$) }

%%

Definition :: { ParsedDefinition }
            : Attributes Modules { ParsedDefinition (reverse $2) $1 }

Attributes :: { ParsedAttributes }
	    : '[' ']' { Attributes [] }
            | '[' AttributeList ']' { reverse $2 }

AttributeList :: {[(AttributeName, AttributeValue)]}
	    : Pattern { [ attributeFromPattern $1] }
            | AttributeList ',' Pattern { attributeFromPattern $3 : $1 }

Modules :: {[ParsedModule]}
         : Module { [$1] }
         | Modules Module { $2 : $1 }

Module :: {ParsedModule}
        : module ident Sentences endmodule Attributes
          { mkModule $2 $5 $3 }
        | module ident endmodule Attributes
          { mkModule $2 $4 [] }

Sentences :: {[ParsedSentence]}
	   : Sentence { [$1] }
           | Sentences Sentence { $2 : $1 }

Sentence :: {ParsedSentence}
	  : import ident Attributes
            { SentenceImport {- ($2, $3) -} }
          | sort Id SortVariables Attributes
            { SentenceSort ParsedSort{name=$2, sortVars=$3, isHooked=False, attributes=$4} }
          | hookedSort Id SortVariables Attributes
            { SentenceSort ParsedSort{name=$2, sortVars=$3, isHooked=True, attributes=$4} }
          | symbol SymbolHead SortsParen ':' Sort Attributes
            { SentenceSymbol ParsedSymbol{name=$2, argSorts=$3, sort=$5, isHooked=False, attributes=$6} }
          | hookedSymbol SymbolHead SortsParen ':' Sort Attributes
            { SentenceSymbol ParsedSymbol{name=$2, argSorts=$3, sort=$5, isHooked=True, attributes=$6} }
          | alias AliasHead SortsParen ':' Sort where Application ':=' Pattern Attributes
            { SentenceAlias {- $2 $3 $5 $7 $9 $10 -} }
          | axiom SortVariables Pattern Attributes
            { SentenceAxiom ParsedAxiom{axiom=$2, sortVars=$3, attributes=$4} }
          | claim SortVariables Pattern Attributes
            { SentenceClaim {- ParsedClaim ParsedAxiom{axiom=$2, sortVars=$3, attributes=$4} -} }

Id :: { Id }
    : ident { Id $1 }

AliasHead :: { () }
	   : Id SortVariables { () {- -} }

SymbolHead :: { [Sort] -> Sort -> Bool -> ParsedAttributes -> ParsedSymbol }
	    : Id SortVariables { ParsedSymbol $1 $2 }

SortVariables :: { [Id] }
	       : '{' SortVariableList '}' { reverse $2 }
               | '{' '}' { [] }

SortVariableList :: { [SortVariable] }
		  : SortVariable { [$1] }
                  | SortVariableList ',' SortVariable { $3 : $1 }

SortVariable :: { Id }
	      : Id { $1 }

SortsParen :: { [Sort] }
       : '(' SortList ')' { reverse $2 }
       | '(' ')' { [] }

SortsBrace :: { [Sort] }
	    : '{' SortList '}' { reverse $2 }
            | '{' '}' { [] }

SortList :: { [Sort] }
	  : Sort { [$1] }
          | SortList ',' Sort { $3 : $1 }

Sort :: { Sort }
      : Id { SortVar $1) }
      | Id SortsBrace { SortApp $1 $2 }

Pattern :: { KorePattern }
	 : ElementVariable { uncurry KJEVar $1 }
	 | SetVariable { uncurry KJSVar $1 }
         | ApplicationPattern { $1 }
         | StringLiteralPattern { $1 }

ElementVariable :: { (Id, Sort) }
	         : ident ':' Sort { ($1, $3) }
SetVariable :: { (Id, Sort) }
	     : setIdent ':' Sort { ($1, $3) }
SomeVariable :: { KorePattern }
	      : ident ':' Sort { KJEVar $1 $3 }
	      | setIdent ':' Sort { KJSVar $1 $3 }

StringLiteralPattern :: { KorePattern }
		      : StringLiteral { KJString $1 }

StringLiteral :: { Text }
               : string { $1 }

ApplicationPattern :: { KorePattern }
		    : leftAssoc '{' '}' '(' ident SortsBrace NePatterns ')'
                      { mkAssoc True $5 $6 $7 }
		    | leftAssoc '{' '}' '(' and SortsBrace NePatterns ')'
                      { mkAssoc True $5 $6 $7 }
		    | leftAssoc '{' '}' '(' or SortsBrace NePatterns ')'
                      { mkAssoc True $5 $6 $7 }
		    | leftAssoc '{' '}' '(' implies SortsBrace NePatterns ')'
                      { mkAssoc True $5 $6 $7 }
		    | leftAssoc '{' '}' '(' iff SortsBrace NePatterns ')'
                      { mkAssoc True $5 $6 $7 }
                    | rightAssoc '{' '}' '(' ident SortsBrace NePatterns ')'
                      { mkAssoc False $5 $6 $7 }
                    | rightAssoc '{' '}' '(' and SortsBrace NePatterns ')'
                      { mkAssoc False $5 $6 $7 }
                    | rightAssoc '{' '}' '(' or SortsBrace NePatterns ')'
                      { mkAssoc False $5 $6 $7 }
                    | rightAssoc '{' '}' '(' implies SortsBrace NePatterns ')'
                      { mkAssoc False $5 $6 $7 }
                    | rightAssoc '{' '}' '(' iff SortsBrace NePatterns ')'
                      { mkAssoc False $5 $6 $7 }
                    | top '{' Sort '}' '(' ')'
                      { KJTop {sort = $3} }
                    | bottom '{' Sort '}' '(' ')'
                      { KJBottom {sort = $3} }
                    | not '{' Sort '}' '(' Pattern ')'
                      { KJNot{sort = $3, arg = $6} }
                    | and '{' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJAnd{sort = $3, first = $6, second = $8} }
                    | or '{' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJOr{sort = $3, first = $6, second = $8} }
                    | implies '{' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJImplies{sort = $3, first = $6, second = $8} }
                    | iff '{' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJIff{sort = $3, first = $6, second = $8} }
                    | exists '{' Sort '}' '(' ElementVariable ',' Pattern ')'
                      { KJExists{sort = $3, var = $6, varSort = _, arg = $8} }
                    | forall '{' Sort '}' '(' ElementVariable ',' Pattern ')'
                      { KJForall{sort = $3, var = $6, varSort = _, arg = $8} }
                    | mu '{' '}' '(' SetVariable ',' Pattern ')'
                      { KJMu{var = $5, varSort = _, arg = $7} }
                    | nu '{' '}' '(' SetVariable ',' Pattern ')'
                      { KJNu{var = $5, varSort = _, arg = $7} }
                    | ceil '{' Sort ',' Sort '}' '(' Pattern ')'
                      { KJCeil{argSort = $3, sort = $5, arg = $8} }
                    | floor '{' Sort ',' Sort '}' '(' Pattern ')'
                      { KJFloor{argSort = $3, sort = $5, arg = $8} }
                    | equals '{' Sort ',' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJEquals{argSort = $3, sort = $5, first = $8, second = $10} }
                    | in '{' Sort ',' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJIn{argSort = $3, sort = $5, first = $8, second = $10} }
                    | next '{' Sort '}' '(' Pattern ')'
                      { KJNext{ sort = $3, dest = $6} }
                    | rewrites '{' Sort '}' '(' Pattern ',' Pattern ')'
                      { KJRewrites{sort = $3, source = $6, dest = $8} }
                    | dv '{' Sort '}' '(' StringLiteral')'
                      { KJDV{sort = $3, value = $6} }
                    | Id SortsBrace Patterns
                      { KJApp{name = $1, sorts = $2, args = $3} }

Application :: { () }
	     : Id SortsBrace SomeVariables { () }

SomeVariables :: { [KorePattern] }
	       : '(' SomeVariableList ')' { reverse $2 }
               | '(' ')' { [] }

SomeVariableList :: { [KorePattern] }
		  : SomeVariable { [$1] }
                  | SomeVariableList ',' SomeVariable { $3 : $1 }

Patterns :: { [KorePattern] }
	  : NePatterns { $1 }
          | '(' ')' { [] }

NePatterns :: { [KorePattern] }
	    : '(' PatternList ')' { reverse $2 }

PatternList :: { [KorePattern] }
             : Pattern { [$1] }
	     | PatternList ',' Pattern { $3 : $1 }

{

-- | helpers for parsing module components
data ParsedSentence
    =
      SentenceImport -- (Json.Id, ParsedAttributes)
    | SentenceSort ParsedSort
    | SentenceSymbol ParsedSymbol
    | SentenceAlias -- ParsedAlias
    | SentenceAxiom ParsedAxiom
    | SentenceClaim -- ParsedClaim
    deriving (Eq, Show)

mkModule :: Json.Id -> [ParsedSentence] -> ParsedModule
mkModule name sentences
--     = ParsedModule {name, imports, sorts, symbols, aliases, axioms, claims}
    = ParsedModule {name, sorts, symbols, axioms}
  where
    (sorts, symbols, axioms) = foldl' collect ([], [], []) sentences
    -- intentionally reversing the list
    collect ::
        (ParsedSort, ParsedSymbol, ParsedAxiom) ->
        ParsedSentence ->
        (ParsedSort, ParsedSymbol, ParsedAxiom)
    collect acc@(!sorts, !symbols, !axioms) = \case
        SentenceSort s -> (s:sorts, symbols, axioms)
        SentenceSymbol s -> (sorts, s:symbols, axioms)
        SentenceAxiom a -> (sorts, symbols, a:axioms)
        _other -> acc

-- helper to parse attributes
attributeFromPattern :: KorePattern -> (AttributeName, AttributeValue)
attributeFromPattern KJApp {name, sorts = [], args = []}
    = (name, Nothing)
attributeFromPattern KJApp {name, sorts = [], args = [KJString{value}]}
    = (name, Just value)
attributeFromPattern badPat
    = error $ "Unexpected attribute shape: " <> show badPat

{- | Expand a \left-assoc or \right-assoc directive into a ParsedPattern. First
argument is True for \left-assoc and False for \right-assoc.
-}
mkAssoc :: Bool -> Token -> [Sort] -> [KorePatterh] -> KorePattern
mkAssoc = undefined
-- mkAssoc True id sorts ps = foldl1' (mkApply id sorts) ps
-- mkAssoc False id sorts ps = foldr1 (mkApply id sorts) ps


--


type Parser a = FilePath -> Text -> Either String a

{- | Helper function for parsing a particular NonTerminal in the grammar. The
function specified by the %name directive for that NonTerminal should be passed
as the first argument to the function. -}
parseNonTerminal :: Alex a -> Parser a
parseNonTerminal a fp s = runAlex fp (Text.encodeUtf8 s) a

{-
-- Functions for parsing specific NonTerminals.

parseAliasHead :: Parser Alias
parseAliasHead = parseNonTerminal aliasHeadStart

parseAttributes :: Parser Attributes
parseAttributes = parseNonTerminal attributesStart

parseDefinition :: Parser ParsedDefinition
parseDefinition = parseNonTerminal definitionStart

parseElementVariable :: Parser (Variable (ElementVariableName VariableName))
parseElementVariable = parseNonTerminal elementVariableStart

parseId :: Parser Id
parseId = parseNonTerminal idStart

parseModule :: Parser ParsedModule
parseModule = parseNonTerminal moduleStart

parsePattern :: Parser ParsedPattern
parsePattern = parseNonTerminal patternStart

parseSentence :: Parser ParsedSentence
parseSentence = parseNonTerminal sentenceStart

parseSetVariable :: Parser (Variable (SetVariableName VariableName))
parseSetVariable = parseNonTerminal setVariableStart

parseSort :: Parser Sort
parseSort = parseNonTerminal sortStart

parseSortsParen :: Parser [Sort]
parseSortsParen = parseNonTerminal sortsParenStart

parseSortVariable :: Parser SortVariable
parseSortVariable = parseNonTerminal sortVariableStart

parseSortVariables :: Parser [SortVariable]
parseSortVariables = parseNonTerminal sortVariablesStart

parseStringLiteral :: Parser StringLiteral
parseStringLiteral = parseNonTerminal stringLiteralStart

parseSymbolHead :: Parser Symbol
parseSymbolHead = parseNonTerminal symbolHeadStart

{- | Reports a parsing error if parsing fails. Called by code generated by
Happy.
-}
happyError :: Token -> Alex a
happyError (Token (AlexPn fp _ line column) t) =
    alexError fp line column ("unexpected token " ++ (show t))

{-
Uses 'parseVariableCounter' to get the 'counter' for the 'Id', if any. Creates
a VariableName.
-}
getVariableName :: Id -> VariableName
getVariableName identifier =
    let (base, counter) = parseVariableCounter identifier
     in VariableName{base, counter}

-- | Read the fresh name counter (if any) from the end of an 'Id'.
parseVariableCounter :: Id -> (Id, Maybe (Sup Natural))
parseVariableCounter identifier@Id{getId, idLocation}
    -- Cases:
    -- suffix is empty: no counter, Id is not changed
    | Text.null suffix = (identifier, Nothing)
    -- suffix is all zeros: counter is zero, Id has final zero stripped
    | Text.null nonZeros =
        ( Id{idLocation, getId = base <> Text.init zeros}
        , Just (Element 0)
        )
    -- suffix is some zeros followed by non-zeros:
    --   read the counter from the non-zeros, Id is base + zeros
    | otherwise =
        ( Id{idLocation, getId = base <> zeros}
        , (Just . Element) (read $ Text.unpack nonZeros)
        )
  where
    base = Text.dropWhileEnd Char.isDigit getId
    suffix = Text.drop (Text.length base) getId
    zeros = Text.takeWhile (== '0') suffix
    nonZeros = Text.drop (Text.length zeros) suffix

-- | Construct a ParsedPattern from its indivdiual operands.
embedParsedPattern :: (PatternF VariableName) ParsedPattern -> ParsedPattern
embedParsedPattern patternBase = asPattern (mempty :< patternBase)

{- | Create an Id from a Token. Uses current location of that Token to create
an AstLocation.
-}
mkId :: Token -> Id
mkId tok@(Token (AlexPn fileName _ line column) _) =
    Id { getId = getIdentName tok
       , idLocation = AstLocationFile $ FileLocation{fileName, line, column}
       }

-- | Create a VariableName from a Token by using mkId and getVariableName.
mkVariableName :: Token -> VariableName
mkVariableName = getVariableName . mkId

{- | Helper function to expand a \\left-assoc or \\right-assoc directive for
a particular type of pattern. Only implemented for Application patterns and
built-in patterns with one sort parameter and two children of the same sort as
the result. Namely, \\and, \\or, \\implies, and \\iff. Designed to be passed to
foldl1' or foldr1.
-}
mkApply :: Token -> [Sort] -> ParsedPattern -> ParsedPattern -> ParsedPattern
mkApply tok@(Token _ TokenAnd) [andSort] andFirst andSecond =
    embedParsedPattern $ AndF And{andSort, andFirst, andSecond}
mkApply tok@(Token _ TokenOr) [orSort] orFirst orSecond =
    embedParsedPattern $ OrF Or{orSort, orFirst, orSecond}
mkApply tok@(Token _ TokenImplies) [impliesSort] impliesFirst impliesSecond =
    embedParsedPattern $ ImpliesF Implies{impliesSort, impliesFirst, impliesSecond}
mkApply tok@(Token _ TokenIff) [iffSort] iffFirst iffSecond =
    embedParsedPattern $ IffF Iff{iffSort, iffFirst, iffSecond}
mkApply tok@(Token _ (TokenIdent _)) sorts first second =
    embedParsedPattern $ ApplicationF Application
       { applicationSymbolOrAlias = SymbolOrAlias { symbolOrAliasConstructor = mkId tok
                                                  , symbolOrAliasParams = sorts
                                                  }
       , applicationChildren = [first, second]
       }
-}
}
