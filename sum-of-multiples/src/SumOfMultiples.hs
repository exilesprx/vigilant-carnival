module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (unique list)
 where
  list = [x | x <- [1 .. limit - 1], y <- factors, y /= 0, x `mod` y == 0]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs)
  | x `elem` xs = unique xs
  | otherwise = x : unique xs
