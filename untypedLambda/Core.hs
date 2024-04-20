module Core(Term(..), NlTerm(..), Context, ev1, eval, nmlOrd) where

data Term =
    TmVar String
  | TmAbs String Term
  | TmApp Term Term
  deriving (Show, Eq)

data NlTerm =
    NlVar (Int,Int) -- (value,ctxLen)... ctxLen parameter may be irrelevant
                    -- given how this implementation is happening...
  | NlAbs String NlTerm -- _ hint scope
  | NlApp NlTerm NlTerm
  deriving (Show, Eq)

type Context = [String]

tmShift :: Int -> Int -> NlTerm -> NlTerm
tmShift c d (NlVar (v,l))
  | v >= c = NlVar (v+d,l+d)
  | otherwise = NlVar (v,l+d)
tmShift c d (NlAbs h t) = NlAbs h (tmShift (c+1) d t)
tmShift c d (NlApp t1 t2) = NlApp (tmShift c d t1) (tmShift c d t2)

subs :: Int -> NlTerm -> NlTerm -> NlTerm
subs j s (NlVar (v,l))
  | v == j = s
  | otherwise = NlVar (v,l)
subs j s (NlAbs h t) = NlAbs h (subs (j+1) (tmShift 0 1 s) t)
subs j s (NlApp t1 t2) = NlApp (subs j s t1) (subs j s t2)

tmSubsTop :: NlTerm -> NlTerm -> NlTerm -- defining it just to shorten the code
                                        -- on the ev1 function...
tmSubsTop s t = tmShift 0 (-1) (subs 0 (tmShift 0 1 s) t)

ev1 :: NlTerm -> Maybe NlTerm
ev1 (NlApp (NlAbs h t12) t2)
  | isval t2 = Just (tmSubsTop t2 t12)
  | otherwise = 
      let m = ev1 t2
      in case m of Nothing -> Nothing
                   Just t2' -> Just (NlApp (NlAbs h t12) t2')
ev1 (NlApp t1 t2) =
  let m = ev1 t1
  in case m of Nothing -> Nothing
               Just t1' -> Just (NlApp t1' t2)
ev1 _ = Nothing

eval :: NlTerm -> NlTerm
eval t =
  let m = ev1 t in maybe t eval m

isval :: NlTerm -> Bool
isval (NlAbs _ _) = True
isval _ = False

--------- Normal order evaluation ------------

nmlOrd1 :: NlTerm -> Maybe NlTerm
nmlOrd1 (NlApp (NlAbs h t12) t2) = Just (tmSubsTop t2 t12)
nmlOrd1 (NlApp t1 t2) =
  let (m1,m2) = (nmlOrd1 t1, nmlOrd1 t2)
  in case (m1,m2) of (Nothing,Nothing) -> Nothing
                     (Nothing, Just t2') -> Just (NlApp t1 t2')
                     (Just t1',_) -> Just (NlApp t1' t2)
nmlOrd1 (NlAbs h t) =
  let m = nmlOrd1 t 
  in case m of Nothing -> Nothing
               Just t' -> Just (NlAbs h t')
nmlOrd1 _ = Nothing

nmlOrd :: NlTerm -> NlTerm
nmlOrd t = 
  let m = nmlOrd1 t in maybe t nmlOrd m
