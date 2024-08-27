import Core

type Variable = String

type Context = [(Variable, Binding)]

data Binding = NameBind | VarBind Ty

addBinding :: Context -> Variable -> Binding -> Context
addBinding ctx var bind = (var,bind):ctx

-- each variable may be bound at most once in a given context.

getVars :: Context -> [Variable]
getVars [] = []
getVars ((name,b):ctx) = name:getVars ctx

-- quicksort with a different name, for sorting variables.

sortVars :: [Variable] -> [Variable]
sortVars [] = []
sortVars (i:is) =
  let leq = [j| j <- is, j <= i]
      gt = [j| j <- is, j > i]
  in (sortVars leq) ++ [i] ++ (sortVars gt)

-- in a sorted list of vars, it's easier to check for repetitions

hasReps :: [Variable] -> Bool
hasReps [] = False
hasReps [i] = False
hasReps (i:j:vs) =
  i == j || hasReps (j:vs)

getTypeFromContext :: Context -> Variable -> Ty
getTypeFromContext ctx i =
  let vars = getVars ctx
      tv = (elem i vars) && not (hasReps (sortVars vars))
  in if tv then getType ctx i
     else error "variable out of range, or multiply bound"

-- assumes the variable is bound exactly once in the context
-- not necessarily...
getBinding :: Context -> Variable -> Binding
getBinding [] _ = error "empty context"
getBinding ((j,b):ctx) i
  | i == j = b
  | otherwise = getBinding ctx i

getType :: Context -> Variable -> Ty
getType ctx i =
  let bdg = getBinding ctx i
  in case bdg of NameBind -> error "name bind instead of type bind."
                 VarBind t -> t
