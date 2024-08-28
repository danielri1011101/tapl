data Ty = TyBool | TyArr Ty Ty

data Term = -- nameless terms.
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmVar (Int,Int) -- (dist,ctxlen)
  | TmAbs String Ty Term
  | TmApp Term Term

data NdTerm = -- named terms.
    NdTrue
  | NdFalse
  | NdIf NdTerm NdTerm NdTerm
  | NdVar String
  | NdAbs String Ty NdTerm
  | NdApp NdTerm NdTerm
