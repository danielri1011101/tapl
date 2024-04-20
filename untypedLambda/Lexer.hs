module Lexer (Token(..), tokenize) where

data Token =
    VAR String
  | LAMBDA
  | DOT
  | LP
  | RP
  | EOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':cs) = LP:tokenize cs
tokenize (')':cs) = RP:tokenize cs
tokenize ('.':cs) = DOT:tokenize cs
tokenize (c:cs)
  | c `elem` ['a'..'z'] =
      let (nm,r) = cutName (c:cs)
      in if nm == "lambda" then LAMBDA:tokenize r 
         else VAR nm:tokenize r
  | c `elem` ['\n', '\t', ' '] = tokenize cs
  | otherwise = [EOF]

cutName :: String -> (String, String)
cutName cs = (getName cs, getRdr cs)

getName :: String -> String
getName cs =
  let nameCs = ['a'..'z'] ++ ['0'..'9'] ++ "'"
  in takeWhile (flip elem nameCs) cs

getRdr :: String -> String
getRdr [] = []
getRdr (c:cs) =
  let nameCs = ['a'..'z'] ++ ['0'..'9'] ++ "'"
  in if c `elem` nameCs then getRdr cs
     else c:cs
