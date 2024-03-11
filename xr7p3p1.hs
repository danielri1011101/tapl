module Lambda (LTerm(..), toString, 
               printtm, 
               pickfreshname) where

-- 09.03.24
-- Removing obsolete comments and code lines.

data LTerm a b =
    TmVar (a,a)         -- second component is the _total length_ of the context 
                        -- in which the variable occurs.
  | TmAbs b (LTerm a b) -- the argument of type b is a _hint_ for the name 
                        -- of the bound variable.
  | TmApp (LTerm a b) (LTerm a b)
  deriving (Show, Eq)  
 
type Context = [String]


printtm :: Context -> LTerm Int String -> String
printtm ctx (TmVar (n,len)) 
  | len == length ctx = index2name ctx n
  | otherwise = error "[bad index]"
printtm ctx (TmAbs hint t) =
  let (ctx', hint') = pickfreshname ctx hint
  in "(lambda " ++ hint' ++ ". " ++ printtm ctx' t ++ ")"
printtm ctx (TmApp t1 t2) =
  "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"



index2name :: Context -> Int -> String
index2name ctx i
  | i < 0 = error "[bad index]"
  | i >= length ctx = error "[bad index]"
  | otherwise = ctx !! i



pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx name
  | elem name ctx =
      let newname = name ++ "'"
          newctx = newname : ctx
      in (newctx, newname)
  | otherwise = (name : ctx, name)


walk :: Int -> Int -> LTerm Int String -> LTerm Int String
walk c d (TmVar (n,len))
  | n >= c = TmVar (n+d,len+d)
  | otherwise = TmVar (n,len+d)
walk c d (TmAbs hint t) = TmAbs hint (walk (c+1) d t)
walk c d (TmApp t1 t2) = TmApp (walk c d t1) (walk c d t2)



termShift :: Int -> LTerm Int String -> LTerm Int String
termShift = walk 0 



termSubst :: Int -> LTerm Int String -> LTerm Int String -> LTerm Int String
termSubst j s (TmVar (k,len))
  | j == k = s
  | otherwise = TmVar (k,len)
termSubst j s (TmAbs hint t) = TmAbs hint (termSubst (j+1) 
                                            (termShift 1 s) t)
termSubst j s (TmApp t1 t2) = TmApp (termSubst j s t1) (termSubst j s t2)



isval :: LTerm Int String -> Bool
isval (TmAbs _ _) = True
isval _ = False



termSubstTop :: LTerm Int String -> LTerm Int String -> LTerm Int String
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)



bigstepEval :: LTerm Int String -> Maybe (LTerm Int String) -- can it be rewritten with _where_ constructs?
bigstepEval t
  | isval t = Just t
bigstepEval (TmApp t1 t2)
  | maybe 
      False 
        isval ( let v1 = bigstepEval t1
                    v2 = bigstepEval t2
                in case (v1,v2) of (_,Nothing) -> Nothing
                                   (Nothing,_) -> Nothing
                                   (Just u1, Just u2) ->
                                      bigstepEval (termSubstTop u2 u1)
              ) = let v1 = bigstepEval t1
                      v2 = bigstepEval t2
                  in case (v1,v2) of (_,Nothing) -> Nothing
                                     (Nothing,_) -> Nothing
                                     (Just u1, Just u2) ->
                                        Just (bigstepEval (termSubstTop u2 u1))
bigstepEval _ = Nothing
