module Acronym (abbreviate) where

import Data.Char (isLower, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = concat [grabUppers x | x <- words (replaceDashes xs)]

replaceDashes :: String -> String
replaceDashes [] = []
replaceDashes (x : xs)
  | x `elem` "-" = ' ' : replaceDashes xs
  | otherwise = x : replaceDashes xs

grabUppers :: String -> String
grabUppers [] = []
grabUppers xs
  | all isUpper xs = [head xs]
  | all isLower xs = [toUpper (head xs)]
  | otherwise = filter isUpper xs
