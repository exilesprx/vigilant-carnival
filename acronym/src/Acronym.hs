module Acronym (abbreviate) where

import Data.Char (isLetter, toUpper)

abbreviate :: String -> String
abbreviate xs = [toUpper (head x) | x <- words (replacePunctuation xs)]

replacePunctuation :: String -> String
replacePunctuation [] = []
replacePunctuation (x : xs)
  | (not . isLetter) x = ' ' : replacePunctuation xs
  | otherwise = x : replacePunctuation xs
