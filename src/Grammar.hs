module Grammar
    ( NonterminalId
    , LexemeId
    , ParserGrammar
    , GrammarCombination
    , GrammarTerm(..)
    , LexerGrammar
    , Lexeme(..)
    , parseGrammarFile
    , isTerminal
    , isNonterminal
    , termName
    ) where

import qualified Data.Map.Lazy as M
import Text.Parsec.Prim
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)

import GrammarLexer

type Parser = Parsec [(SourcePos, GrammarLexeme)] ()

type NonterminalId = String
type ParserGrammar = M.Map NonterminalId [GrammarCombination]
type GrammarCombination = [GrammarTerm]

data GrammarTerm = Nonterminal NonterminalId
                 | Terminal LexemeId
                   deriving (Show, Eq)

isTerminal, isNonterminal :: GrammarTerm -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False
isNonterminal = not . isTerminal

termName :: GrammarTerm -> String
termName (Nonterminal s) = s
termName (Terminal s) = s

type LexemeId = String
type LexerGrammar = M.Map LexemeId AnnotatedLexeme

data AnnotatedLexeme = ChannelMain Lexeme
                     | ChannelHidden Lexeme
                       deriving (Show, Eq)

data Lexeme = StringToken String
            | RegexToken String
              deriving (Show, Eq)

-- Common things for grammar tokens recognition
tokenIf :: Show a => (a -> Bool) -> Parsec [(SourcePos, a)] u a
tokenIf cond = token show fst (\(pos, t) -> if cond t then Just t else Nothing)

colon, semicolon, divider, nonTerm, term, stringLiteral, regexLiteral :: Parser GrammarLexeme
colon     = tokenIf (== Colon)
semicolon = tokenIf (== Semicolon)
divider   = tokenIf (== Divider)
nonTerm   = tokenIf isNonTerm
    where isNonTerm (NonTerm _) = True
          isNonTerm _ = False
term = tokenIf isTerm
    where isTerm (Term _) = True
          isTerm _ = False
stringLiteral = tokenIf isStringLiteral
    where isStringLiteral (StringLiteral _) = True
          isStringLiteral _ = False
regexLiteral = tokenIf isRegexLiteral
    where isRegexLiteral (RegexLiteral _) = True
          isRegexLiteral _ = False

nonterminalId, lexemeId, stringToken, regexToken :: Parser String
nonterminalId = show <$> nonTerm
lexemeId      = show <$> term
stringToken   = show <$> stringLiteral
regexToken    = show <$> regexLiteral


-- Grammar for parser
parserGrammar :: Parser ParserGrammar
parserGrammar = M.fromList <$> many1 parserRule

parserRule :: Parser (NonterminalId, [GrammarCombination])
parserRule = do
  nid  <- nonterminalId <* colon
  alts <- sepBy1 grammarCombination divider <* semicolon
  return (nid, alts)

grammarCombination :: Parser GrammarCombination
grammarCombination = many1 grammarTerm

grammarTerm :: Parser GrammarTerm
grammarTerm = try (Nonterminal <$> nonterminalId)
              <|> (Terminal <$> lexemeId)

-- Grammar for lexer
lexerGrammar :: Parser LexerGrammar
lexerGrammar = M.fromList <$> many1 lexerRule

lexerRule :: Parser (LexemeId, AnnotatedLexeme)
lexerRule = do
  lid <- lexemeId <* colon
  lx  <- annotatedLexeme <* semicolon
  return (lid, lx)

annotatedLexeme :: Parser AnnotatedLexeme
annotatedLexeme = try (ChannelHidden <$> (tokenHidden *> lexeme))
                  <|> (ChannelMain <$> lexeme)

tokenHidden :: Parser GrammarLexeme
tokenHidden = tokenIf (== Term "HIDDEN")

lexeme :: Parser Lexeme
lexeme = try (StringToken <$> stringToken)
         <|> (RegexToken <$> regexToken)

parseGrammarFile :: [(SourcePos, GrammarLexeme)] -> Either ParseError (ParserGrammar, LexerGrammar)
parseGrammarFile = parse parseBoth ""
    where parseBoth = do
            pg <- parserGrammar
            lg <- lexerGrammar
            return (pg, lg)
