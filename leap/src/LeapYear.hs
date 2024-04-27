module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = divisableBy 4 && (not (divisableBy 100) || divisableBy 400)
  where
    divisableBy num = year `mod` num == 0
