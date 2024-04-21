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



toString :: (Show a) => LTerm a String -> String
toString (TmVar x) = show $ fst x
toString (TmAbs hint t) = "(l. " ++ toString t ++ ")"
toString (TmApp t1 t2) = "(" ++ toString t1 ++ " " ++ toString t2 ++ ")"



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
      pickfreshname ctx (name ++ "'")
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



eval1 :: LTerm Int String -> Maybe (LTerm Int String)
eval1 (TmApp (TmAbs hint t1) t2)
  | isval t2 = Just (termSubstTop t2 t1)
  | otherwise =
      let ans = eval1 t2
      in case ans of Nothing -> Nothing
                     Just s2 ->
                       Just (TmApp (TmAbs hint t1) s2)
eval1 (TmApp t1 t2) =
  let ans = eval1 t1
  in case ans of Nothing -> Nothing
                 Just s1 -> Just (TmApp s1 t2)  
eval1 _ = Nothing



eval :: LTerm Int String -> LTerm Int String
eval t = 
  let ans = eval1 t
  in case ans of Nothing -> t
                 Just s -> eval s
                        
