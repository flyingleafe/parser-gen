{-# LANGUAGE UnicodeSyntax #-}
module Generator
    ( GeneratorConfig(..)
    , generateParserSource
    ) where

import Data.List
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer
import Prelude.Unicode
import Data.List.Unicode
import Data.Either.Utils

import Grammar
import GrammarProcessing

type GeneratorOutput = String
data GeneratorConfig = GC
    { parserName ∷ String
    , pGrammar   ∷ ParserGrammar
    , lGrammar   ∷ LexerGrammar
    , gFIRST     ∷ GrammarTable
    , gFOLLOW    ∷ GrammarTable
    }

type Generator = ReaderT GeneratorConfig (WriterT GeneratorOutput (Either String))

-- Utils
tabulate ∷ Int → String → String
tabulate n s = replicate n ' ' ++ s

-- Names generators
tokenDatatypeName, tokenTypeDatatypeName, moduleName ∷ Generator String
tokenDatatypeName = (++ "LexerToken") <$> asks parserName
tokenTypeDatatypeName = (++ "LexerTokenType") <$> asks parserName
astDatatypeName = (++ "ParserASTNode") <$> asks parserName
parserTypeName = (++ "Parser") <$> asks parserName
moduleName = (++ "Parser") <$> asks parserName

-- Generators of datatypes
fileHeader ∷ Generator ()
fileHeader = do
  mname ← moduleName
  tell $ "module " ++ mname ++ " where\n\
\\n\
\import Data.List\n\
\import Data.Ord\n\
\import Control.Monad.Trans\n\
\import Control.Monad.Trans.State\n\
\import Text.Regex.Posix\n"

lexerDatatype ∷ Generator ()
lexerDatatype = do
  tdn  ← tokenDatatypeName
  ttdn ← tokenTypeDatatypeName
  css  ← tokenTypeConstructors
  tell $ "data " ++ tdn ++ " = " ++ tdn ++ " { text :: String, type' :: " ++ ttdn ++ " }\n"
  tell $ "data " ++ ttdn ++ " = "
  tell $ concat (intersperse " | " $ map snd css)
  tell $ " deriving Eq\n"

tokenShowInstance ∷ Generator ()
tokenShowInstance = do
  tdn ← tokenDatatypeName
  tell $ "instance Show " ++ tdn ++ " where\n"
  tell $ "    show = text\n"

tokenTypeShowInstance ∷ Generator ()
tokenTypeShowInstance = do
  ttdn ← tokenTypeDatatypeName
  css  ← tokenTypeConstructors
  tell $ "instance Show " ++ ttdn ++ " where\n"
  forM_ css $ \(name, cs) → tell $ "    show " ++ cs ++ " = \"" ++ name ++ "\"\n"

tokenTypeConstructors ∷ Generator [(String, String)]
tokenTypeConstructors = do
  name ← asks parserName
  lgr  ← asks lGrammar
  let lexemes = M.keys lgr ∪ ["EOF"]
  return $ zip lexemes $ map (getTokenTypeConstructor name) lexemes

getTokenTypeConstructor ∷ String → LexemeId → String
getTokenTypeConstructor name id = name ++ "Lexer_" ++ if id ≡ "$" then "EOF" else id

astDatatype ∷ Generator ()
astDatatype = do
  pdn ← astDatatypeName
  tell $ "data " ++ pdn ++ " = " ++ pdn ++ "Nonterm String [" ++ pdn ++ "] | " ++ pdn ++ "Term String deriving Show\n"

parserType ∷ Generator ()
parserType = do
  ptn ← parserTypeName
  tdn ← tokenDatatypeName
  tell $ "type " ++ ptn ++ " = StateT [" ++ tdn ++ "] (Either String)\n"

-- Generators of parsers
parserFunctions ∷ Generator ()
parserFunctions = do
  ptn ← parserTypeName
  tdn ← tokenDatatypeName
  ttdn ← tokenTypeDatatypeName
  tell $ "curToken :: " ++ ptn ++ " " ++ tdn ++ "\n"
  tell $ "curToken = gets head\n\n"
  tell $ "consumeToken :: " ++ ttdn ++ " -> " ++ ptn ++ " " ++ tdn ++ "\n"
  tell $ "consumeToken ttype = do { ct <- curToken; if type' ct /= ttype then "
  tell $ "lift $ Left (\"Expected \" ++ show ttype ++ \" but found \" ++ show ct) else "
  tell $ "modify tail >> return ct; }\n"

getRules ∷ NonterminalId → Generator [GrammarCombination]
getRules nt = do
  pgr ← asks pGrammar
  let errStr = "Nonterminal `" ++ nt ++ "` is not found in grammar"
  maybeToEither errStr $ M.lookup nt pgr

parserMainCaseSwitch ∷ NonterminalId → Generator ()
parserMainCaseSwitch nonterm = do
  pname  ← asks parserName
  ptn    ← parserTypeName
  atn    ← astDatatypeName
  tdn    ← tokenDatatypeName
  first  ← asks gFIRST
  follow ← asks (flip getList nonterm ∘ gFOLLOW)
  -- pgr    ← asks pGrammar
  rules  ← getRules nonterm

  let fname = "parse_" ++ nonterm
  tell $ fname ++ " :: " ++ ptn ++ " " ++ atn ++ "\n"
  tell $ fname ++ " = do\n"
  tell $ "    curt <- curToken\n"
  tell $ "    children <- switchCase curt\n"
  tell $ "    return $ " ++ atn ++ "Nonterm \"" ++ nonterm ++ "\" children\n"
  tell $ "  where switchCase curt\n"

  forM_ rules $ \rule → do
    let lexemes = getFIRST first rule \\ ["EPSILON"]
    if length lexemes ≡ 0 then return ()
    else do
      let constrs = map (getTokenTypeConstructor pname) lexemes
      tell $ tabulate 12 $ "| type' curt `elem` [" ++ (intercalate ", " constrs) ++ "] = sequence ["

      let actionSequence = map makeAction rule
          makeAction (Nonterminal nt) = "parse_" ++ nt
          makeAction (Terminal lid) = let constr = getTokenTypeConstructor pname lid
                                      in "consumeToken " ++ constr ++ " >>= return . " ++ atn ++ "Term . text"

      tell $ (intercalate ", " actionSequence) ++ "]\n"

  let hasEpsilon = any (\rule → "EPSILON" ∈ getFIRST first rule) rules
  if hasEpsilon ∧ length follow ≢ 0 then do
    let constrs = map (getTokenTypeConstructor pname) follow
    tell $ tabulate 12 "| type' curt `elem` [" ++ (intercalate ", " constrs) ++ "]"
    tell $ " = return [" ++ atn ++ "Term \"EPSILON\"]\n"
  else return ()

  tell $ tabulate 12 "| otherwise = lift $ Left \"unexpected char\"\n"

allCaseSwitches ∷ Generator ()
allCaseSwitches = do
  nonterms ← asks (M.keys ∘ pGrammar)
  forM_ nonterms $ \nt → parserMainCaseSwitch nt >> tell "\n"

lexerFunction ∷ Generator ()
lexerFunction = do
  pname ← asks parserName
  tdn   ← tokenDatatypeName
  tell $ "maybeLength :: Maybe " ++ tdn ++ " -> Int\n"
  tell $ "maybeLength Nothing = 0\n"
  tell $ "maybeLength (Just tok) = length (text tok)\n"
  tell $ "\n"
  tell $ "getLongest :: [Maybe " ++ tdn ++ "] -> Maybe " ++ tdn ++ "\n"
  tell $ "getLongest = maximumBy (comparing maybeLength)\n"
  tell $ "\n"
  tell $ "runLexer :: String -> Maybe ([" ++ tdn ++ "], [" ++ tdn ++ "])\n"
  tell $ "runLexer [] = Just ([" ++ tdn ++ " \"$\" " ++ getTokenTypeConstructor pname "$" ++ "], [])\n"
  tell $ "runLexer s = do\n"
  tell $ "  case hiddenMatch s of\n"
  tell $ "    Just tok -> do { (main, hid) <- runLexer (drop (length (text tok)) s); return (main, tok : hid) }\n"
  tell $ "    Nothing -> do\n"
  tell $ "      longest <- getLongest (matchStr s ++ matchRegex s)\n"
  tell $ "      (main, hid) <- runLexer (drop (length (text longest)) s)\n"
  tell $ "      return (longest : main, hid)\n"
  tell $ "\n"
  tell $ "matchStr :: String -> [Maybe " ++ tdn ++ "]\n"
  tell $ "matchStr s = [ "

  lgr ← asks lGrammar
  let mainStrToks = takeStrTokens $ takeMain lgr
      matchings   = map makeStrMatch mainStrToks

      makeStrMatch (lid, str) = "stripPrefix \"" ++ str ++ "\" s >> return (" ++
                                tdn ++ " \"" ++ str ++  "\" " ++ getTokenTypeConstructor pname lid ++ ")"

  tell $ intercalate "\n             , " matchings
  tell $ "\n             ]\n\n"

  tell $ "matchRegex :: String -> [Maybe " ++ tdn ++ "]\n"
  tell $ "matchRegex s = [ "

  let mainRegexToks = takeRegexTokens $ takeMain lgr
      matchings     = map makeRegexMatch mainRegexToks
      makeRegexMatch (lid, rgx) = "let (b, m, _) = s =~ \"" ++ rgx ++ "\" :: (String, String, String) in " ++
                                  "if length b /= 0 then Nothing else Just (" ++
                                  tdn ++ " m " ++ getTokenTypeConstructor pname lid ++ ")"

  tell $ intercalate "\n               , " matchings
  tell $ "\n               ]\n\n"

  tell $ "hiddenMatch :: String -> Maybe " ++ tdn ++ "\n"
  tell $ "hiddenMatch s = getLongest (hiddenMatchStr s ++ hiddenMatchRegex s)\n\n"
  tell $ "hiddenMatchStr :: String -> [Maybe " ++ tdn ++ "]\n"
  tell $ "hiddenMatchStr s = [ "

  let hiddenStrToks = takeStrTokens $ takeHidden lgr
      matchings     = map makeStrMatch hiddenStrToks

  tell $ intercalate "\n                   , " matchings
  tell $ "\n                   ]\n\n"

  tell $ "hiddenMatchRegex :: String -> [Maybe " ++ tdn ++ "]\n"
  tell $ "hiddenMatchRegex s = [ "

  let hiddenRegexToks = takeRegexTokens $ takeHidden lgr
      matchings     = map makeRegexMatch hiddenRegexToks

  tell $ intercalate "\n                     , " matchings
  tell $ "\n                     ]\n"


unwrapChan ∷ (LexemeId, AnnotatedLexeme) → (LexemeId, Lexeme)
unwrapChan (lid, ChannelMain lex) = (lid, lex)
unwrapChan (lid, ChannelHidden lex) = (lid, lex)

unwrapLexeme ∷ (LexemeId, Lexeme) → (LexemeId, String)
unwrapLexeme (lid, StringToken s) = (lid, s)
unwrapLexeme (lid, RegexToken s) = (lid, s)

takeMain ∷ LexerGrammar → [(LexemeId, Lexeme)]
takeMain lgr = map unwrapChan $ filter isMain $ M.toList lgr
    where isMain (_, ChannelMain _) = True
          isMain _ = False

takeHidden ∷ LexerGrammar → [(LexemeId, Lexeme)]
takeHidden lgr = map unwrapChan $ filter isHidden $ M.toList lgr
    where isHidden (_, ChannelHidden _) = True
          isHidden _ = False

takeStrTokens ∷ [(LexemeId, Lexeme)] → [(LexemeId, String)]
takeStrTokens = map unwrapLexeme ∘ filter isStrTok
    where isStrTok (_, StringToken _) = True
          isStrTok _ = False

takeRegexTokens ∷ [(LexemeId, Lexeme)] → [(LexemeId, String)]
takeRegexTokens = map unwrapLexeme ∘ filter isRegTok
    where isRegTok (_, RegexToken _) = True
          isRegTok _ = False

allSource ∷ Generator ()
allSource = sequence_ $ intersperse (tell "\n")
  [ fileHeader
  , lexerDatatype
  , tokenShowInstance
  , tokenTypeShowInstance
  , astDatatype
  , parserType
  , parserFunctions
  , lexerFunction
  , allCaseSwitches
  ]

generateParserSource ∷ GeneratorConfig → Either String GeneratorOutput
generateParserSource cfg = execWriterT $ runReaderT allSource cfg
