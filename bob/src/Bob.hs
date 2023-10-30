module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | all isUpper (filter isLetter xs) && last xs /= '?' = "Whoa, chill out!"
  | null (filter (not . isSpace) xs) = "Fine. Be that way!"
  | all isUpper (filter isLetter xs) && last xs == '?' = "Calm down, I know what I'm doing!"
  | last xs == '?' = "Sure."
  | otherwise = "Whatever."