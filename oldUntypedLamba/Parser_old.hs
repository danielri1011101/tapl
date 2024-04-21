module Parser (Term(..), Parse(..)) where

import Lexer

data Term =
    TmVar String
  | TmApp Term Term
  | TmAbs String Term -- String field is the bound variable's name.
  deriving (Show, Eq)

data Parse =
    AST Term
  | Error String
  deriving (Show, Eq)

fullparse :: String -> Term
fullparse = parse . tokenize
     
parse :: [Token] -> Term
parse [] = error "empty token list"
parse [t]
  | tIsVar t = TmVar (tokenVal t)
  | otherwise = error "the single token is not a variable"
parse (t:ts)
  | tIsVar t = parseApp (t:ts)
  | t == LAMBDA = parseAbs (t:ts)
  | t == LP = parseUntilRP ts
  | otherwise = error "syntax error"

parseApp :: [Token] -> Term
parseApp (VAR w:tks) =
  let t1 = asscLeft (getVars (VAR w:tks))
      r = tvsTail (VAR w:tks)
  in if null r then t1
     else TmApp t1 (parse r)
parseApp _ = error "not an application"

parseAbs :: [Token] -> Term
parseAbs (LAMBDA:VAR w:DOT:tks) =
  TmAbs w (parse tks)
parseAbs _ = error "not an abstraction"

parseUntilRP :: [Token] -> Term
parseUntilRP tks =
  let tks' = takeWhile (/=RP) tks
  in parse tks'

parenSplit :: [Token] -> ([Token], [Token])
parenSplit tks = firstMatched (0,[],tks)

firstMatched :: (Int, [Token], [Token]) -> ([Token], [Token])
firstMatched (n,_,_)
  | n < 0 = error "unmatched parentheses"
firstMatched (n, tks, t:tks')
  | not (elem t [LP,RP]) = firstMatched (n, tks ++ [t], tks')
  | t == LP = firstMatched (n+1, tks ++ [t], tks')
  | t == RP && n == 1 = (tks ++ [t], tks')
  | otherwise = firstMatched (n-1, tks ++ [t], tks')

tIsVar :: Token -> Bool
tIsVar (VAR _) = True
tIsVar _ = False

asscLeft :: [Term] -> Term
asscLeft (t:tms) =
  foldl (\t1 t2 -> TmApp t1 t2) t tms

getVars :: [Token] -> [Term]
getVars [] = []
getVars (t:ts)
  | tIsVar t = TmVar (tokenVal t):getVars ts
  | otherwise = []

tvsTail :: [Token] -> [Token]
tvsTail [] = []
tvsTail (t:ts)
  | tIsVar t = tvsTail ts
  | otherwise = t:ts

tokenVal :: Token -> String
tokenVal (VAR w) = w
tokenVal LAMBDA = "lambda"
tokenVal LP = "("
tokenVal RP = ")"
tokenVal DOT = "."
tokenVal EOF = "<EOF>"
