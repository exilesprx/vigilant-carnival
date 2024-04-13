module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

initial :: Map Nucleotide Int
initial = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right initial
nucleotideCounts (x : xs)
  -- I need to figure out how to increment the count of A in the map while recursing through the string
  | x == 'A' = Right $ Map.insertWith (+) A 1 xs
  | x == 'C' = Right initial
  | x == 'G' = Right initial
  | x == 'T' = Right initial
  | otherwise = Left "error"
