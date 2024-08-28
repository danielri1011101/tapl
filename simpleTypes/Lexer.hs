module Lexer (Token(..), tokenize) where

data Token = -- haven't included the primitive true, false, if!!
    VAR String
  | IF
  | THN
  | ELS
  | TRU
  | FLS
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
  | c `elem` ['a'..'z'] = let (nm,r) = cutName (c:cs) in dstk nm:tokenize r
  | c `elem` ['\n',' ','\t'] = tokenize cs
  | otherwise = [EOF]

dstk :: String -> Token -- decide string token
dstk "if" = IF
dstk "then" = THN
dstk "else" = ELS
dstk "true" = TRU
dstk "false" = FLS
dstk nm = VAR nm

cutName :: String -> (String, String)
cutName cs = (getName cs, getRdr cs)

getName :: String -> String
getName =
  let nameCs = ['a'..'z'] ++ ['0'..'9'] ++ "'"
  in takeWhile (`elem` nameCs)


getRdr :: String -> String 
getRdr [] = []
getRdr (c:cs) =
  let nameCs = ['a'..'z'] ++ ['0'..'9'] ++ "'"
  in if c `elem` nameCs then getRdr cs
     else c:cs
