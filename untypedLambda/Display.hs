module Display () where

import Parser

toForest :: Term -> String
toForest (TmVar x) = "[" ++ x ++ "]"
toForest (TmAbs v t) = "[$"++"\92lambda$" ++ v ++ toForest t ++ "]"
toForest (TmApp t1 t2) = "[app" ++ toForest t1 ++ toForest t2 ++ "]"

main = do
  writeFile "parseTree.tex" 
    ("\92documentclass{article}" ++
     "\n\92usepackage[linguistics]{forest}" ++
     "\n\92begin{document}" ++
     "\n\92begin{forest}\n    ")
  putStrLn "Write a lambda-term to be parsed:"
  tm <- getLine
  appendFile "parseTree.tex" (toForest (fullparse tm))
  appendFile "parseTree.tex" 
    ("\n\92end{forest}" ++
     "\n\92end{document}")
