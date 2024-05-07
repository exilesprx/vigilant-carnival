module Acronym (abbreviate) where

import Data.Char (toUpper)

-- first slip on spaces
-- then grab first letter of each word
-- then make them uppercase
-- then concatenate them
abbreviate :: String -> String
abbreviate xs = [toUpper (head x) | x <- words xs]
