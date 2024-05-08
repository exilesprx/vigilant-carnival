module Acronym (abbreviate) where

import Data.Char (toUpper)

abbreviate :: String -> String
abbreviate xs = [toUpper (head x) | x <- words (replacePunctuation xs)]

replacePunctuation :: String -> String
replacePunctuation [] = []
replacePunctuation (x : xs)
  | x `elem` "-_" = ' ' : replacePunctuation xs
  | otherwise = x : replacePunctuation xs
