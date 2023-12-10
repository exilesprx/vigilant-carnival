module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = cal n 0
  where 
    cal :: Integer -> Integer -> Maybe Integer
    cal m i
      | m == 1 = Just i
      | even m = cal (m `div` 2) (i + 1)
      | otherwise = cal (m `div` 3 + 1) (i + 1)