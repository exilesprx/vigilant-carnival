module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = calcSteps n 0
  where 
    calcSteps :: Integer -> Integer -> Maybe Integer
    calcSteps m i
      | m == 1 = Just i
      | m <= 0 = Nothing
      | even m = calcSteps (m `div` 2) (i + 1)
      | otherwise = calcSteps (m * 3 + 1) (i + 1)

callarzAlt :: Integer -> Maybe Integer
callarzAlt n
  | n <= 0 = Nothing
  | n == 1 = Just 1
  | even n = fmap(+1) $ callarzAlt (n `div` 2)
  | otherwise = fmap(+1) $ callarzAlt (n * 3 + 1)
  -- basically once 1 is, when we "unwrap" the function chain we add 1 for each function on the stack