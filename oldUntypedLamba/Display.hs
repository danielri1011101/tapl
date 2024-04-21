module Display () where

import Parser

toForest :: Term -> String
toForest (TmVar x) = "[" ++ x ++ "]"
toForest (TmAbs v t) = "[$"++"\92lambda$" ++ v ++ toForest t ++ "]"
toForest (TmApp t1 t2) = "[app" ++ toForest t1 ++ toForest t2 ++ "]"
