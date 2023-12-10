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