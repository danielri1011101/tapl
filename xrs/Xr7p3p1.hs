import Core

-- Exercise 7.3.1: Implement evaluation of terms using big-step semantics.
-- To work, it must be in the same folder as the Core module of the compiler.
-- can it be written using _where_ constructs instead of _let_ bindings and _case_ expressions?

bigstepEval :: NlTerm -> Maybe NlTerm 
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
                                      bigstepEval (tmSubsTop u2 u1)
              ) = let v1 = bigstepEval t1
                      v2 = bigstepEval t2
                  in case (v1,v2) of (_,Nothing) -> Nothing
                                     (Nothing,_) -> Nothing
                                     (Just u1, Just u2) ->
                                        Just (bigstepEval (tmSubsTop u2 u1))
bigstepEval _ = Nothing
