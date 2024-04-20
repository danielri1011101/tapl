module Display (toForest, printTm, printNl, removeNames, restoreNames) where

import Core

import Parser

toForest :: Term -> String
toForest (TmVar x) = "[" ++ x ++ "]"
toForest (TmAbs v t) = "[$"++"\92lambda$" ++ v ++ toForest t ++ "]"
toForest (TmApp t1 t2) = "[app" ++ toForest t1 ++ toForest t2 ++ "]"

printTm :: Term -> String
printTm (TmVar vname) = vname
printTm (TmAbs vname t) = "lambda " ++ vname ++ ". " ++ printTm t
printTm (TmApp t1 t2) = 
  let app = TmApp t1 t2 in printLApp (appToList app)

appToList :: Term -> [Term]
appToList (TmApp (TmVar x1) t2) = let vtm = TmVar x1 in [vtm,t2]
appToList (TmApp (TmAbs x t1) t2) = let abs = TmAbs x t1 in [abs,t2]
appToList (TmApp t1 t2) = appToList t1 ++ [t2]
appToList _ = []

printLApp :: [Term] -> String
printLApp [TmVar x] = x
printLApp [TmAbs x t] = 
  let abs = TmAbs x t in printTm abs
printLApp [TmApp t1 t2] =
  let app = TmApp t1 t2 in "(" ++ printTm app ++ ")"
printLApp ((TmVar x):ts) = x ++ " " ++ printLApp ts
printLApp ((TmAbs x t):ts) =
  let abs = TmAbs x t in "(" ++ printTm abs ++ ") " ++ printLApp ts
printLApp ((TmApp t1 t2):ts) = 
  let app = TmApp t1 t2 in  "(" ++ printTm app ++ ") " ++ printLApp ts
--printLApp _ = "" ... any problem with this non-exhaustive pattern?

----------------------------------------------------------------------------------------------------------------
---------------- Removing and restoring names ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

removeNames :: Term -> Context -> NlTerm
removeNames (TmVar vname) ctx = NlVar (name2index vname ctx, length ctx)
removeNames (TmAbs vname t) ctx = NlAbs vname (removeNames t (vname:ctx))
removeNames (TmApp t1 t2) ctx = NlApp (removeNames t1 ctx) (removeNames t2 ctx)

name2index :: String -> Context -> Int
name2index _ [] = error "name not found!"
name2index name (x:xs)
  | name == x = 0
  | otherwise = 1 + name2index name xs

restoreNames :: NlTerm -> Context -> Term
restoreNames (NlVar (v,l)) ctx
  | l > length ctx = error "context too small"
  | otherwise =
      let var = index2name v ctx in TmVar var
restoreNames (NlAbs hint t) ctx =
  let (name, ctx') = freshName hint ctx in TmAbs name (restoreNames t ctx')
restoreNames (NlApp t1 t2) ctx = TmApp (restoreNames t1 ctx) (restoreNames t2 ctx)

index2name :: Int -> Context -> String
index2name = flip (!!)

freshName :: String -> Context -> (String, Context)
freshName x ctx
  | x `elem` ctx =
      let x' = x ++ "'" in freshName x' ctx
  | otherwise = (x, x:ctx)

----------------------------------------------------------------------------------------------------------------
---------------- Printing nameless terms -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

printNl :: NlTerm -> String
printNl (NlVar (v,l)) = show v
printNl (NlAbs h t) = "l. " ++ printNl t
printNl (NlApp t1 t2) =
  let a = NlApp t1 t2 in pNlApp (nlA2List a)

pNlApp :: [NlTerm] -> String
pNlApp [NlVar (v,l)] = show v
pNlApp [NlAbs h t] =
  let a = NlAbs h t in printNl a
pNlApp [NlApp t1 t2] =
  let a = NlApp t1 t2 in "(" ++ printNl a ++ ")"
pNlApp ((NlVar (v,l)):ts) = show v ++ " " ++ pNlApp ts
pNlApp ((NlAbs h t):ts) =
  let a = NlAbs h t in "(" ++ printNl a ++ ") " ++ pNlApp ts
pNlApp ((NlApp t1 t2):ts) =
  let a = NlApp t1 t2 in "(" ++ printNl a ++ ") " ++ pNlApp ts

nlA2List :: NlTerm -> [NlTerm]
nlA2List (NlApp (NlVar (v,l)) t2) =
  let nlv = NlVar (v,l) in [nlv,t2]
nlA2List (NlApp (NlAbs h t) t2) =
  let nla = NlAbs h t in [nla,t2]
nlA2List (NlApp t1 t2) =
  let nla = NlApp t1 t2 in nlA2List t1 ++ [t2]
nlA2List _ = []


