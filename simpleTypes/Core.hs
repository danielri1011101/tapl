data Ty = TyBool | TyArr Ty Ty

data Term = -- all terms are nameless...
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmVar (Int,Int)
  | TmAbs String Ty Term
  | TmApp Term Term
