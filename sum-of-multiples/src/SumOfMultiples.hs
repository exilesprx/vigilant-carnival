module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1 .. limit - 1], (isMultiple x) factors]
 where
  isMultiple :: Integer -> [Integer] -> Bool
  isMultiple x = any (\factor -> factor /= 0 && x `mod` factor == 0)

-- the any function can be used to check if any of the
-- factors are multiples of x which removes duplicates
-- because once found, the other factors are not checked
