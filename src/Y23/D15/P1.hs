module Y23.D15.P1 where

import qualified Data.Char as C
import qualified Data.List.Split as S

parse :: String -> [String]
parse input = S.splitWhen (== ',') $ filter (/= '\n') input

hash :: String -> Int
hash = foldl (\a c -> ((a + C.ord c) * 17) `mod` 256) 0

solve :: String -> Int
solve input = sum $ map hash $ parse input

main :: IO ()
main = do
  input <- readFile "src/Y23/D15/input.txt"
  -- input <- readFile "src/Y23/D15/example.txt"
  print $ solve input
