{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Grammar
import GrammarLexer
import GrammarProcessing
import Generator
import System.Environment
import System.IO
import Data.Either.Combinators

generateParser ∷ String → Either String String
generateParser input = do
  (pgr, lgr) ← mapLeft show $ runLexer input >>= parseGrammarFile
  (pgr', first, follow) ← processGrammar pgr
  let config = GC { parserName = "Variables"
                  , pGrammar  = pgr'
                  , lGrammar  = lgr
                  , gFIRST    = first
                  , gFOLLOW   = follow
                  }
  generateParserSource config


main :: IO ()
main = do
  args ← getArgs
  if length args == 0 then putStrLn "No file name specified" >> return ()
  else do
    let filename = head args
    file ← openFile filename ReadMode
    input ← hGetContents file
    case generateParser input of
      Left err → putStrLn err
      Right res → putStrLn res
