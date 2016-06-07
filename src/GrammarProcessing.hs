{-# LANGUAGE UnicodeSyntax #-}
module GrammarProcessing
    ( GrammarTable
    , removeNongenerating
    , removeUnreachable
    , removeUseless
    , processGrammar
    , findFIRST
    , findFOLLOW
    , getFIRST
    , getList
    ) where

import Prelude.Unicode
import Data.List.Unicode
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Debug.Trace

import Grammar

type GrammarTable = M.Map NonterminalId [LexemeId]

getList ∷ Ord k ⇒ M.Map k [v] → k → [v]
getList tbl s = concat $ maybeToList $ M.lookup s tbl

getRules ∷ ParserGrammar → NonterminalId → [GrammarCombination]
getRules gr s = concat $ map combs $ maybeToList $ M.lookup s gr

takeNonterminals ∷ [GrammarTerm] → [NonterminalId]
takeNonterminals = map termName ∘ filter isNonterminal

removeNongenerating ∷ ParserGrammar → ParserGrammar
removeNongenerating grammar = foldGrammar [(nt, rules) | (nt, rules) ← grammar'
                                          , nt ∈ generating
                                          , nontermsIn generating rules] rvMap
    where generating         = takeLastChanged $ iterate findGenerating termOnly
          termOnly           = [nt | (nt, rules) ← grammar', all isTerminal rules]
          findGenerating gen = gen ∪ [nt | (nt, rules) ← grammar'
                                     , nontermsIn gen rules]
          nontermsIn list    = all (∈ list) ∘ takeNonterminals
          (grammar', rvMap)  = unfoldGrammar grammar

removeUnreachable ∷ ParserGrammar → ParserGrammar
removeUnreachable grammar = M.filterWithKey (\k _ → k ∈ reachable) grammar
    where reachable = reach [] "start"
          reach reached nt = foldl' markReached ([nt] ∪ reached) $ concatMap takeNonterminals $ getRules grammar nt
          markReached reached nt = if nt ∈ reached then reached else reach reached nt

removeUseless ∷ ParserGrammar → ParserGrammar
removeUseless = removeUnreachable ∘ removeNongenerating

getFIRST ∷ GrammarTable → GrammarCombination → [LexemeId]
getFIRST tbl [] = ["EPSILON"]
getFIRST tbl (a:as) = fstA ∪ fstAS
    where fstA = getFIRST' a
          fstAS = if "EPSILON" ∈ fstA then getFIRST tbl as else []
          getFIRST' (Terminal s) = [s]
          getFIRST' (Nonterminal s) = getList tbl s
          getFIRST' (Action _) = ["EPSILON"]

findFIRST ∷ ParserGrammar → GrammarTable
findFIRST grammar = takeLastChanged $ iterate updateFIRST M.empty
    where updateFIRST tbl = foldl' updateSingle tbl grammar'
          updateSingle tbl (nt, rule) = M.insertWith (∪) nt (getFIRST tbl rule) tbl
          (grammar', _) = unfoldGrammar grammar

findFOLLOW ∷ ParserGrammar → GrammarTable → GrammarTable
findFOLLOW grammar gFIRST = takeLastChanged $ iterate updateFOLLOW $ M.fromList [("start", ["$"])]
    where updateFOLLOW tbl = foldl' updateSingle tbl grammar'
          updateSingle tbl (ntA, rule) = foldl' updateTail tbl $ getNTTails rule
              where getNTTails [] = []
                    getNTTails (Nonterminal s : rest) = (s, rest) : getNTTails rest
                    getNTTails (_ : rest) = getNTTails rest
                    updateTail tbl (ntB, rest) = M.insertWith (∪) ntB ((fstRest \\ ["EPSILON"]) ∪ followA) tbl
                        where fstRest = getFIRST gFIRST rest
                              followA = if "EPSILON" ∈ fstRest then getList tbl ntA else []
          (grammar', _) = unfoldGrammar grammar

processGrammar ∷ ParserGrammar → Either String (ParserGrammar, GrammarTable, GrammarTable)
processGrammar grammar =
    if M.notMember "start" grammar
    then fail "No 'start' symbol detected in grammar"
    else do
      let grammar' = removeUseless grammar
          gFIRST   = findFIRST grammar'
          gFOLLOW  = findFOLLOW grammar' gFIRST
      return (grammar', gFIRST, gFOLLOW)

unfoldGrammar ∷ ParserGrammar → ([(NonterminalId, GrammarCombination)], M.Map NonterminalId [(String, String)])
unfoldGrammar gr = (unfoldGrammar' $ M.toList gr, M.map returnVals gr)
    where unfoldGrammar' [] = []
          unfoldGrammar' ((nt, ParserRule _ []) : rest) = unfoldGrammar' rest
          unfoldGrammar' ((nt, ParserRule rv (r:rs)) : rest) = (nt, r) : unfoldGrammar' ((nt, ParserRule rv rs) : rest)

foldGrammar ∷ [(NonterminalId, GrammarCombination)] → M.Map NonterminalId [(String, String)] → ParserGrammar
foldGrammar gr rvmap = M.foldlWithKey glueRule M.empty folded
    where folded = foldl' upd M.empty gr
          upd tbl (nt, rule) = M.insertWith (++) nt [rule] tbl
          glueRule res nt rules = M.insert nt (ParserRule rvs' rules) res
              where rvs' = getList rvmap nt

takeLastChanged ∷ Eq a ⇒ [a] → a
takeLastChanged [] = error "wut"
takeLastChanged [x] = x
takeLastChanged (x:y:xs) = if x ≡ y then x else takeLastChanged (y:xs)
