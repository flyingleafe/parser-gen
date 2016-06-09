module Main where

import Data.Tree
import Data.List
import Data.Ord
import Control.Monad.State
import System.IO
import ObfuscationParser
import qualified Data.Map as M

class ToTree a where
    toTree :: a -> Tree String

instance ToTree ObfuscationParserASTNode where
    toTree (ObfuscationParserASTNodeNonterm s chs) = Node s $ map toTree chs
    toTree (ObfuscationParserASTNodeTerm s) = Node s []

initState :: ObfuscationParserInnerState
initState = ObfuscationParserInnerState
            { _input = []
            , scopes = [M.empty]
            , genNum = 0
            , tokenRewriter = []
            }

writeTokens :: [ObfuscationLexerToken] -> IO ()
writeTokens = mapM_ (putStr . show)

rewriteTokens :: [ObfuscationLexerToken] -> TokenRewriter -> IO ()
rewriteTokens [] _ = return ()
rewriteTokens toks [] = writeTokens toks
rewriteTokens (t : ts) (a : as) = do
  let cur = _index t
      toRewrite = index a
  if cur < toRewrite then putStr (show t) >> rewriteTokens ts (a : as)
  else case a of
         Replace _ str -> putStr str >> rewriteTokens ts as

zipStreams :: [ObfuscationLexerToken] -> [ObfuscationLexerToken] -> [ObfuscationLexerToken]
zipStreams [] ts = ts
zipStreams ts [] = ts
zipStreams (a : as) (b : bs) = if _index a < _index b
                               then a : zipStreams as (b : bs)
                               else b : zipStreams (a : as) bs

main :: IO ()
main = do
  input <- getContents
  case runLexer input of
    Nothing -> putStrLn "Lexer error"
    Just (mainch, hidch) -> do
      case runStateT parse_start (initState { _input = mainch }) of
        Left err -> putStrLn err
        Right (_, st) -> rewriteTokens (zipStreams mainch hidch) $ sortBy (comparing index) $ tokenRewriter st
