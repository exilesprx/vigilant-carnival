module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | all (\x -> isUpper x || isSpace x) (init xs) && last xs /= '?' = "Whoa, chill out!"
  | all isSpace xs = "Fine. Be that way!"
  | all (\x -> isUpper x || isSpace x) (init xs) && last xs == '?' = "Calm down, I know what I'm doing!"
  | last xs == '?' = "Sure."
  | otherwise = "Whatever."