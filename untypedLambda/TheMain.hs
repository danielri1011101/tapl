import Display

import Parser

import Core

main = do
  writeFile "parseTree.tex" 
    ("\92documentclass{article}" ++
     "\n\92usepackage[linguistics]{forest}" ++
     "\n\92begin{document}" ++
     "\n\92begin{forest}\n    ")
  putStrLn "Write a term to evaluate:" -- extend to l-tms by asking for
                                              -- a context.
  tm <- getLine
  let parsed = fullParse tm
  prectx <- extendCtx []
  let ctx = reverse prectx
  putStrLn "Ok, hit RETURN to continue..."
  l <- getLine
  let nless = removeNames parsed ctx
  let ans = eval nless
  let ndans = restoreNames ans ctx
  let ans' = nmlOrd nless
  let ndans' = restoreNames ans' ctx
  appendFile "parseTree.tex" (toForest parsed)
  appendFile "parseTree.tex" 
    ("\n\92end{forest}" ++
     "\n\92end{document}")
  putStrLn "This is the nameless representation of your term:"
  putStrLn $ printNl nless
  putStrLn "Hit RETURN key to continue..."
  l <- getLine
  putStrLn "This is the result of call-by-value evaluation:"
  putStrLn $ printTm ndans
  putStrLn "This is the result of normal-order evaluation:"
  putStrLn $ printTm ndans'

extendCtx :: Context -> IO Context
extendCtx c = do
  putStrLn "Write a name to add to the context:"
  nm <- getLine
  if null nm then return c
  else extendCtx (nm:c)
