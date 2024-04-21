module Ccore(Term(..), NlTerm(..)) where

data Term =
    TmVar String
  | TmAbs String Term
  | TmApp Term Term
  deriving (Show, Eq)

data NlTerm =
    NlVar (Int,Int) -- _ (varIndex,ctxLength)
  | NlAbs String NlTerm -- _ nameHint scope
  | NlApp NlTerm NlTerm
  deriving (Show, Eq)
