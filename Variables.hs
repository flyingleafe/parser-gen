module Main where

import Data.Tree
import Control.Monad.State
import System.IO
import VariablesParser

class ToTree a where
    toTree :: a -> Tree String

instance ToTree VariablesParserASTNode where
    toTree (VariablesParserASTNodeNonterm s chs) = Node s $ map toTree chs
    toTree (VariablesParserASTNodeTerm s) = Node s []

main :: IO ()
main = do
  input <- getContents
  case runLexer input of
    Nothing -> putStrLn "Lexer error"
    Just (mainch, _) -> do
      case evalStateT parse_start mainch of
        Left err -> putStrLn err
        Right node -> putStrLn $ drawTree $ toTree node
