module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | containsLetter && allUppercaseLetter && last xs /= '?' = "Whoa, chill out!"
  | all isSpace xs = "Fine. Be that way!"
  | containsLetter && allUppercaseLetter && last xs == '?' = "Calm down, I know what I'm doing!"
  | last trimWhiteSpace == '?' = "Sure."
  | otherwise = "Whatever."
    where
      containsLetter = any isLetter xs
      allUppercaseLetter = all isUpper (filter isLetter xs)
      trimWhiteSpace = filter (not . isSpace) xs