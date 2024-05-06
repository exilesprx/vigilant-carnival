module Grains (square, total) where

import Data.Maybe (fromMaybe)

square :: Integer -> Maybe Integer
square n
  | n <= 0 || n >= 65 = Nothing
  | otherwise = Just (2 ^ (n - 1))

total :: Integer
total = sum (fmap (\x -> fromMaybe 0 (square x)) [1 .. 64])
