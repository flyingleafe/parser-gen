module Grammar
    ( NonterminalId
    , LexemeId
    , ParserGrammar
    , ParserRule(..)
    , GrammarCombination
    , GrammarTerm(..)
    , LexerGrammar
    , AnnotatedLexeme(..)
    , Lexeme(..)
    , parseGrammarFile
    , isTerminal
    , isNonterminal
    , isAction
    , termName
    ) where

import qualified Data.Map.Lazy as M
import Text.Parsec.Prim hiding (State)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Control.Monad.State
import Data.Maybe

import GrammarLexer

type Parser = ParsecT [(SourcePos, GrammarLexeme)] () (State (M.Map String String))

type NonterminalId = String
type ParserGrammar = M.Map NonterminalId ParserRule

data ParserRule = ParserRule { takenParams :: [(String, String)]
                             , returnVals :: [(String, String)]
                             , combs :: [GrammarCombination]
                             } deriving (Show, Eq)

type GrammarCombination = [GrammarTerm]

data GrammarTerm = Nonterminal NonterminalId (Maybe String)
                 | Terminal LexemeId
                 | Action String
                   deriving (Show, Eq)

isTerminal, isNonterminal, isAction :: GrammarTerm -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False
isNonterminal (Nonterminal _ _) = True
isNonterminal _ = False
isAction (Action _) = True
isAction _ = False

termName :: GrammarTerm -> String
termName (Nonterminal s _) = s
termName (Terminal s) = s
termName (Action s) = s

type LexemeId = String
type LexerGrammar = M.Map LexemeId AnnotatedLexeme

data AnnotatedLexeme = ChannelMain Lexeme
                     | ChannelHidden Lexeme
                       deriving (Show, Eq)

data Lexeme = StringToken String
            | RegexToken String
              deriving (Show, Eq)

-- Common things for grammar tokens recognition
tokenIf :: (GrammarLexeme -> Bool) -> Parser GrammarLexeme
tokenIf cond = tokenPrim show (\_ t _  -> fst t) (\(_, t) -> if cond t then Just t else Nothing)

colon, semicolon, divider, nonTerm, term, codeBlock, paramsBlock, stringLiteral, regexLiteral :: Parser GrammarLexeme
colon     = tokenIf (== Colon)
semicolon = tokenIf (== Semicolon)
divider   = tokenIf (== Divider)
comma     = tokenIf (== Comma)
arrow     = tokenIf (== ReturnArrow)
dcolon    = tokenIf (== DoubleColon)
lsq       = tokenIf (== LeftSquare)
rsq       = tokenIf (== RightSquare)
nonTerm   = tokenIf isNonTerm
    where isNonTerm (NonTerm _) = True
          isNonTerm _ = False
term = tokenIf isTerm
    where isTerm (Term _) = True
          isTerm _ = False
codeBlock = tokenIf isCodeBlock
    where isCodeBlock (CodeBlock _) = True
          isCodeBlock _ = False
paramsBlock = tokenIf isParamsBlock
    where isParamsBlock (ParamsBlock _) = True
          isParamsBlock _ = False
stringLiteral = tokenIf isStringLiteral
    where isStringLiteral (StringLiteral _) = True
          isStringLiteral _ = False
regexLiteral = tokenIf isRegexLiteral
    where isRegexLiteral (RegexLiteral _) = True
          isRegexLiteral _ = False

nonterminalId, lexemeId, codeBlockStr, stringToken, regexToken :: Parser String
nonterminalId  = show <$> nonTerm
lexemeId       = show <$> term
codeBlockStr   = show <$> codeBlock
paramsBlockStr = show <$> paramsBlock
stringToken    = show <$> stringLiteral
regexToken     = show <$> regexLiteral

-- Parse header
parserHeader :: Parser String
parserHeader = try (tokenIf (== NonTerm "header") *> codeBlockStr)
                <|> return ""

parserStateData :: Parser String
parserStateData = try (tokenIf (== NonTerm "stateData") *> codeBlockStr)
                  <|> return "_dummy :: ()"

parseCustom :: Parser String
parseCustom = try (tokenIf (== NonTerm "custom") *> codeBlockStr)
              <|> return ""

-- Grammar for parser
parserGrammar :: Parser ParserGrammar
parserGrammar = M.fromList <$> many1 parserRule

parserRule :: Parser (NonterminalId, ParserRule)
parserRule = do
  nid     <- nonterminalId
  tparams <- takenParamsList
  retvals <- retValsList <* colon
  alts    <- sepBy1 grammarCombination divider <* semicolon
  return (nid, ParserRule tparams retvals alts)

takenParamsList :: Parser [(String, String)]
takenParamsList = try (lsq *> varDef `sepBy` comma <* rsq)
                  <|> return []

retValsList :: Parser [(String, String)]
retValsList = try (arrow *> lsq *> varDef `sepBy` comma <* rsq)
              <|> return []

varDef :: Parser (String, String)
varDef = (,) <$> (nonterminalId <* dcolon) <*> lexemeId

grammarCombination :: Parser GrammarCombination
grammarCombination = many1 grammarTerm

grammarTerm :: Parser GrammarTerm
grammarTerm = try (Nonterminal <$> nonterminalId <*> optionMaybe paramsBlockStr)
              <|> try (Terminal <$> lexemeId)
              <|> try inlineTerminal
              <|> Action <$> codeBlockStr

inlineTerminal :: Parser GrammarTerm
inlineTerminal = do
  (StringLiteral s) <- stringLiteral
  newLit <- gets (M.notMember s)
  msize  <- gets M.size
  tokName <- if newLit
             then do
               let tname = "inttok" ++ show msize
               modify $ M.insert s tname
               return tname
             else gets $ fromJust . M.lookup s
  return $ Terminal tokName

-- Grammar for lexer
lexerGrammar :: Parser LexerGrammar
lexerGrammar = do
  explicitGrammar <- M.fromList <$> many1 lexerRule
  stmap <- get
  let implicitGrammar = M.fromList [(key, ChannelMain (StringToken val)) | (val, key) <- M.toList stmap]
  return $ M.union explicitGrammar implicitGrammar

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

parseGrammarFile :: [(SourcePos, GrammarLexeme)] -> Either ParseError (String, String, String, ParserGrammar, LexerGrammar)
parseGrammarFile tokstream = evalState (runParserT parseBoth () "" tokstream) M.empty
    where parseBoth = do
            header <- parserHeader
            stateData <- parserStateData
            custom <- parseCustom
            pg <- parserGrammar
            lg <- lexerGrammar
            return (header, stateData, custom, pg, lg)
