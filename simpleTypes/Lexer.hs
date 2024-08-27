module Lexer (Token(..), tokenize) where

data Token =
    VAR String
  | COLON
  | BOOL
  | ARROW
  | LAMBDA
  | DOT
  | LP
  | RP
  | EOF

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':cs) = LP:tokenize cs
tokenize (')':cs) = RP:tokenize cs
tokenize ('.':cs) = DOT:tokenize cs
tokenize ('\92':cs) = LAMBDA:tokenize cs
tokenize (':':cs) = COLON:tokenize cs
tokenize ('-':'>':cs) = ARROW:tokenize cs
tokenize ('B':'o':'o':'l':cs) = BOOL:tokenize cs
tokenize (c:cs)
  | c `elem` ['a'..'z'] = getName (c:cs)
