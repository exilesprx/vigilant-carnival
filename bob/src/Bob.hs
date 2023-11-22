module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | containsUppercaseChar && last xs /= '?' = "Whoa, chill out!"
  | all isSpace xs = "Fine. Be that way!"
  | containsUppercaseChar && last xs == '?' = "Calm down, I know what I'm doing!"
  | last trimmed == '?' = "Sure."
  | otherwise = "Whatever."
    where
      containsUppercaseChar = any isLetter xs && all isUpper (filter isLetter xs)
      trimmed = filter (not . isSpace) xs