module GrammarLexer
    ( GrammarLexeme(..)
    , runLexer
    ) where

import Data.Char
import Data.Monoid ((<>))
import Control.Applicative ((<*>), (<*), (*>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos)

data GrammarLexeme = NonTerm String
                   | Term String
                   | Typename String
                   | StringLiteral String
                   | RegexLiteral String
                   | CodeBlock String
                   | ParamsBlock String
                   | Colon
                   | Semicolon
                   | Comma
                   | Divider
                   | ReturnArrow
                   | DoubleColon
                   | LeftSquare
                   | RightSquare
                     deriving Eq

instance Show GrammarLexeme where
    show (NonTerm s) = s
    show (Term s) = s
    show (StringLiteral s) = s
    show (RegexLiteral s) = s
    show (CodeBlock s) = s
    show (ParamsBlock s) = s
    show Colon = ":"
    show Semicolon = ";"
    show Comma = ","
    show Divider = "|"
    show ReturnArrow = "->"
    show DoubleColon = "::"
    show LeftSquare = "["
    show RightSquare = "]"

instance Monoid a => Monoid (ParsecT s u m a) where
    mappend a b = mappend <$> a <*> b
    mempty = return mempty

skipSpaces :: Parser a -> Parser a
skipSpaces p = p <* spaces

surround :: Parser a -> Parser b -> Parser b
surround p q = p *> q <* p

parsePos :: Parser a -> Parser (SourcePos, a)
parsePos p = (,) <$> getPosition <*> p

tokenize :: Parser [(SourcePos, GrammarLexeme)]
tokenize = spaces *> many1 (parsePos grammarLexeme) <* eof

grammarLexeme :: Parser GrammarLexeme
grammarLexeme = choice $ map (try . skipSpaces)
                [ nonTerm
                , term
                , stringLiteral
                , regexLiteral
                , codeBlock
                , paramsBlock
                , dcolon
                , semicolon
                , comma
                , divider
                , arrow
                , lsq
                , rsq
                , colon
                , lineComment
                ]

lineComment :: Parser GrammarLexeme
lineComment = string "//" *> many (noneOf "\r\n") *> spaces *> grammarLexeme

nonTerm :: Parser GrammarLexeme
nonTerm = NonTerm <$> ((:) <$> lower <*> many (try alphaNum <|> char '_'))

term :: Parser GrammarLexeme
term = Term <$> ((:) <$> upper <*> many (try alphaNum <|> char '_'))

stringLiteral :: Parser GrammarLexeme
stringLiteral = StringLiteral <$> surround (char '\'') (escapedString "'")

regexLiteral :: Parser GrammarLexeme
regexLiteral = RegexLiteral <$> surround (char '/') (escapedString1 "/")

codeBlock :: Parser GrammarLexeme
codeBlock = CodeBlock <$> (char '{' *> bracesText "{" "}" <* char '}')

paramsBlock :: Parser GrammarLexeme
paramsBlock = ParamsBlock <$> (char '(' *> bracesText "(" ")" <* char ')')

bracesText :: String -> String -> Parser String
bracesText lb rb = noBrace <> (concat <$> many (string lb <> bracesText lb rb <> string rb <> noBrace))
    where noBrace = many (noneOf (lb ++ rb))

colon, semicolon, comma, divider, arrow, rsq, lsq, dcolon :: Parser GrammarLexeme
colon     = char ':' *> pure Colon
semicolon = char ';' *> pure Semicolon
comma     = char ',' *> pure Comma
divider   = char '|' *> pure Divider
lsq       = char '[' *> pure LeftSquare
rsq       = char ']' *> pure RightSquare
arrow     = string "->" *> pure ReturnArrow
dcolon    = string "::" *> pure DoubleColon

escapedChar :: String -> Parser Char
escapedChar s = try (char '\\' *> oneOf ('\\' : s))
                <|> char '\\'

escapedString, escapedString1 :: String -> Parser String
escapedString s  = many (noneOf ('\\' : s) <|> escapedChar s)
escapedString1 s = many1 (noneOf ('\\' : s) <|> escapedChar s)

runLexer :: String -> Either ParseError [(SourcePos, GrammarLexeme)]
runLexer = parse tokenize ""
