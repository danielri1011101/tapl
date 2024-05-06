myfold :: [a] -> (a -> b -> b) -> b -> b
myfold [] _ y0 = y0
myfold (x:xs) f y0 = f x (myfold xs f y0)
