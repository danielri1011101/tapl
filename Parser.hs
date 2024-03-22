module Parser (Term(..)) where

import Lexer

data Term =
    TmVar String
  | TmApp Term Term
  | TmAbs String Term -- String field is the bound variable's name.
  deriving (Show, Eq)

fullparse :: String -> Term
fullparse = parseTm . tokenize
     
parseTm :: [Token] -> Term
parseTm = asscLeft . parse

parse :: [Token] -> [Term]
parse [] = []
parse (t:ts)
  | tIsVar t = TmVar (tokenVal t):parse ts
  | t == LAMBDA = parseAbs (t:ts):[]
  | t == LP = 
      let (tks, r) = parenSplit (t:ts)
          tks' = init (tail tks)
      in parseTm tks':parse r
  | otherwise = error "syntax error"

parseAbs :: [Token] -> Term
parseAbs (LAMBDA:VAR w:DOT:tks) =
  TmAbs w (parseTm tks)
parseAbs _ = error "not an abstraction"

parenSplit :: [Token] -> ([Token], [Token])
parenSplit tks = firstMatched (0,[],tks)

firstMatched :: (Int, [Token], [Token]) -> ([Token], [Token])
firstMatched (n,_,ts)
  | n < 0 = error "unmatched parentheses"
  | n > 0 && null ts = error "was expecting a RP"
firstMatched (n, tks, t:tks')
  | not (elem t [LP,RP]) = firstMatched (n, tks ++ [t], tks')
  | t == LP = firstMatched (n+1, tks ++ [t], tks')
  | t == RP && n == 1 = (tks ++ [t], tks')
  | otherwise = firstMatched (n-1, tks ++ [t], tks')

tIsVar :: Token -> Bool
tIsVar (VAR _) = True
tIsVar _ = False

asscLeft :: [Term] -> Term
asscLeft [] = error "there are no terms"
asscLeft (t:tms) =
  foldl (\t1 t2 -> TmApp t1 t2) t tms

getVars :: [Token] -> [Term]
getVars [] = []
getVars (t:ts)
  | tIsVar t = TmVar (tokenVal t):getVars ts
  | otherwise = []

tokenVal :: Token -> String
tokenVal (VAR w) = w
tokenVal LAMBDA = "lambda"
tokenVal LP = "("
tokenVal RP = ")"
tokenVal DOT = "."
tokenVal EOF = "<EOF>"

