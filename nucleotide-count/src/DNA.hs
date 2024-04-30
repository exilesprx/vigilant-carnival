module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

initial :: Map Nucleotide Int
initial = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right initial
nucleotideCounts (x : xs)
  | x == 'A' = fmap (Map.insertWith (+) A 1) (nucleotideCounts xs) -- we don't need Right the base case provides it
  | x == 'C' = fmap (Map.insertWith (+) C 1) (nucleotideCounts xs)
  | x == 'G' = fmap (Map.insertWith (+) G 1) (nucleotideCounts xs)
  | x == 'T' = fmap (Map.insertWith (+) T 1) (nucleotideCounts xs)
  | otherwise = Left "error"
