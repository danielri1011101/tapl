module Lexer (Token(..), tokenize) where

data Token =
    LP
  | RP
  | DOT
  | LAMBDA
  | VAR String
  | EOF
  deriving (Show, Eq)


-- get a line from stdin, return the corresponding
-- list of tokens.


tokenize :: String -> [Token]
tokenize xs =
  let (t, r) = tokenize1 xs
  in if t == EOF then []
     else t:tokenize r

tokenize1 :: String -> (Token, String)
-- returns one token and the remainder of the string
tokenize1 [] = (EOF, [])
tokenize1 (x:xs)
  | isSkip x = tokenize1 xs
  | x == '(' = (LP, xs)
  | x == ')' = (RP, xs)
  | x == '.' = (DOT, xs)
  | let ls = ['a'..'z'] -- boolean as a let binding...
    in x `elem` ls = tokenizeName (x:xs)
  | otherwise = (EOF, xs)

tokenizeName :: String -> (Token, String)
tokenizeName xs =
  let cs = ['a'..'z'] ++ ['0'..'9'] ++ ['\'']
      p = \c -> elem c cs
      (w, r) = (takeWhile p xs, takeTail p xs)
  in if w == "lambda" then (LAMBDA, r)
     else (VAR w, r)

---------------------------------------------------------------------------
---------------- Helping functions ----------------------------------------
---------------------------------------------------------------------------

takeTail :: (a -> Bool) -> [a] -> [a]
takeTail _ [] = []
takeTail p (x:xs)
  | p x = takeTail p xs 
  | otherwise = x:xs

isSkip :: Char -> Bool
isSkip x =
  let cs = [' ', '\t', '\n', '\r']
  in x `elem` cs

