module Y23.D09.P2 where

import Y23.D09.P1

intrapolate :: [[Int]] -> Int
intrapolate = foldr (\a b -> head a - b) 0

solve2 :: String -> [Int]
solve2 input = map (intrapolate . seqs) (parse input)

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D09/input.txt"
  -- input <- readFile "src/Y23/D09/example.txt"
  print $ solve2 input
  print $ sum $ solve2 input
