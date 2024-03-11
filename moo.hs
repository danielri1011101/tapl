maybeNeg :: Int -> Maybe Int
maybeNeg x
  | x < 0 = Just (-1)
  | otherwise = Nothing

myAbsVal :: Int -> Int
myAbsVal x =
  let ans = maybeNeg x
  in case ans of Nothing -> x
                 Just n -> n*x

-- Simple f'ns for testing combined use of
-- let bindings and case expressions...

maybeNeg' :: Int -> Maybe Int
maybeNeg' x
  | x < 0 = Just (-2)
  | otherwise = Nothing

myAbsVal' :: Int -> Int
myAbsVal' x =
  let ans = maybeNeg' x
  in case ans of Nothing -> x
                 Just n -> n*x
