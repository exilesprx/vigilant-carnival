module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

initial :: Map Nucleotide Int
initial = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right initial
nucleotideCounts (x : xs)
  | x `elem` "ACGT" = fmap (Map.insertWith (+) (toNucleotide x) 1) (nucleotideCounts xs) -- we don't need Right the base case provides it
  | otherwise = Left "error"

toNucleotide :: Char -> Nucleotide
toNucleotide 'A' = A
toNucleotide 'C' = C
toNucleotide 'G' = G
toNucleotide 'T' = T
toNucleotine _ = error "invalid nucleotide"
