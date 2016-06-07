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
                   | StringLiteral String
                   | RegexLiteral String
                   | Colon
                   | Semicolon
                   | Divider
                     deriving Eq

instance Show GrammarLexeme where
    show (NonTerm s) = s
    show (Term s) = s
    show (StringLiteral s) = s
    show (RegexLiteral s) = s
    show Colon = ":"
    show Semicolon = ";"
    show Divider = "|"

skipSpaces :: Parser a -> Parser a
skipSpaces p = p <* spaces

surround :: Parser a -> Parser b -> Parser b
surround p q = p *> q <* p

parsePos :: Parser a -> Parser (SourcePos, a)
parsePos p = (,) <$> getPosition <*> p

tokenize :: Parser [(SourcePos, GrammarLexeme)]
tokenize = spaces *> many1 (skipSpaces $ parsePos grammarLexeme)

grammarLexeme :: Parser GrammarLexeme
grammarLexeme = try nonTerm
                <|> try term
                <|> try stringLiteral
                <|> try regexLiteral
                <|> try colon
                <|> try semicolon
                <|> divider

nonTerm :: Parser GrammarLexeme
nonTerm = NonTerm <$> ((:) <$> lower <*> many (try alphaNum <|> char '_'))

term :: Parser GrammarLexeme
term = Term <$> many1 (try upper <|> char '_')

stringLiteral :: Parser GrammarLexeme
stringLiteral = StringLiteral <$> surround (char '\'') (escapedString "'")

regexLiteral :: Parser GrammarLexeme
regexLiteral = RegexLiteral <$> surround (char '/') (escapedString "/")

colon, semicolon, divider :: Parser GrammarLexeme
colon     = char ':' *> pure Colon
semicolon = char ';' *> pure Semicolon
divider   = char '|' *> pure Divider

escapedChar :: String -> Parser Char
escapedChar s = char '\\' *> oneOf ('\\' : s)

escapedString :: String -> Parser String
escapedString s = many (noneOf s <|> escapedChar s)

runLexer :: String -> Either ParseError [(SourcePos, GrammarLexeme)]
runLexer = parse tokenize ""
