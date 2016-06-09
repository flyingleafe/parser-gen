{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grammar
import GrammarLexer
import GrammarProcessing
import Generator
import System.Environment
import System.IO
import Data.Either.Combinators
import Filesystem.Path
import Filesystem.Path.CurrentOS

generateParser ∷ String → String → Either String String
generateParser pname input = do
  (header, stateData, custom, pgr, lgr) ← mapLeft show $ runLexer input >>= parseGrammarFile
  (pgr', first, follow) ← processGrammar pgr
  let config = GC { parserName   = pname
                  , parserHeader = header
                  , parserState  = stateData
                  , parserCustom = custom
                  , pGrammar     = pgr'
                  , lGrammar     = lgr
                  , gFIRST       = first
                  , gFOLLOW      = follow
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
    case generateParser (encodeString $ basename $ decodeString filename) input of
      Left err → putStrLn err
      Right res → putStrLn res
