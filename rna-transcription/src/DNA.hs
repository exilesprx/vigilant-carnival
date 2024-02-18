module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = replaceDna
  where replaceDna [] = Right []
        replaceDna (x:xs)
          | x == 'G' = Right ['C']
          | x == 'C' = Right ['G']
          | x == 'T' = Right ['A']
          | x == 'A' = Right ['U']
          | otherwise = Left x