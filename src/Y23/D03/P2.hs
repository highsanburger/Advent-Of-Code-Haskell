module Y23.D03.P2 where

import Y23.D03.P1

getAdjacent :: [String] -> [[Int]]
getAdjacent ex = map (map (findElement iex) . around) sex
  where
    iex = indexify $ pad $ map (digitToNumber . map morph) ex
    sex = symbols iex

solve2 :: String -> Int
solve2 inp = sum $ map product $ filter (\arr -> length arr == 2) $ map (filter (/= 0) . rmdups) (getAdjacent $ lines inp)

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D03/input.txt"
  print $ solve2 input

-- WORKS BUT TWO NUMS SIDE BY SIDE BREAKS IT LOL
-- ~20s
