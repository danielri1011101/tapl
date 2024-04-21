module Pparser() where

import Ccore

import Llexer

parseTm :: [Token] -> Term
parseTm = asscLeft . parse

parse :: [Token] -> [Term]
parse [] = []
parse ((VAR nm):tks) = TmVar nm:parse tks
parse (LAMBDA:VAR nm:DOT:tks) = [TmAbs nm (parseTm tks)]
parse (LP:tks) =
  let (m,r) = parenSplit (LP:tks) in parseTm m:parse r
parse _ = error "syntax error"

asscLeft :: [Term] -> Term
asscLeft (tm:tms) = foldl TmApp tm tms

parenSplit :: [Token] -> ([Token], [Token])
parenSplit tks =
  let (pm, r) = firstMatched 0 [] tks in (init (tail pm),r)

firstMatched :: Int -> [Token] -> [Token] -> ([Token], [Token])
firstMatched n _ _
  | n < 0 = error "syntax error: parentheses don't match."
firstMatched 0 t1s [] = (t1s, [])
firstMatched 1 t1s (RP:t2s) = (RP:t1s, t2s)
firstMatched n t1s (t2:t2s)
  | let ps = [LP,RP] 
    in notElem t2 ps = firstMatched n (t2:t1s) t2s
  | t2 == LP = firstMatched (n+1) (t2:t1s) t2s
  | t2 == RP = firstMatched (n-1) (t2:t1s) t2s
firstMatched n t1s []
  | n > 0 = error "syntax error: parentheses don't match."
  | n == 0 = (t1s, [])
firstMatched _ _ _ = error "syntax error."
