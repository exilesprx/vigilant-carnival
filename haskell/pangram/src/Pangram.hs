module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram = (==26) . length . filter(\x -> x `elem` ['a'..'z']) . map toLower