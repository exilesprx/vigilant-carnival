module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = Right $ Map.fromList[(A, 0), (C, 0), (G, 0), (T, 0)]
nucleotideCounts xs = error "You need to implement this function."
