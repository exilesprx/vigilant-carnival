module CollatzConjecture (collatz) where
import System.Console.Haskeline (Interrupt)
import Distribution.Compat.Graph (Node(N))

collatz :: Integer -> Maybe Integer
collatz n = calcSteps n 0
  where
    calcSteps :: Integer -> Integer -> Maybe Integer
    calcSteps m i
      | m == 1 = Just i
      | m <= 0 = Nothing
      | even m = calcSteps (m `div` 2) (i + 1)
      | otherwise = calcSteps (m * 3 + 1) (i + 1)

collarzAlt :: Integer -> Maybe Integer
collarzAlt n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | even n = fmap (+1) $ collarzAlt (n `div` 2)
  | otherwise = fmap (+1) $ collarzAlt (n * 3 + 1)
  -- basically once 1 is, when we "unwrap" the function chain we add 1 for each function on the stack

collarzAlt2 :: Integer -> Maybe Integer
collarzAlt2 n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | even n = fmap(\f -> f + 1) $ collarzAlt2(n `div` 2)
  | otherwise = fmap(\f -> f + 1) $ collarzAlt2(n * 3 + 1)