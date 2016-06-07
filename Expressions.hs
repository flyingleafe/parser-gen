module Main where

import Data.Tree
import Control.Monad.State
import System.IO
import ExpressionsParser

class ToTree a where
    toTree :: a -> Tree String

instance ToTree ExpressionsParserASTNode where
    toTree (ExpressionsParserASTNodeNonterm s chs) = Node s $ map toTree chs
    toTree (ExpressionsParserASTNodeTerm s) = Node s []

main :: IO ()
main = do
  input <- getContents
  case runLexer input of
    Nothing -> putStrLn "Lexer error"
    Just (mainch, _) -> do
      case evalStateT parse_start (ExpressionsParserInnerState mainch ()) of
        Left err -> putStrLn err
        Right (res, node) -> do
                          putStrLn $ "Result: " ++ show (start_val res)
                          putStrLn $ drawTree $ toTree node
