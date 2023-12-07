module Y23.D06.P2 where

import Y23.D06.P1

parse2 :: String -> (Int, Int)
parse2 input = (time, dist)
  where
    time = read $ concat $ tail $ words $ takeWhile (/= '\n') input
    dist = read $ concat $ tail $ words $ dropWhile (/= '\n') input

solve2 :: String -> Int
solve2 inp = ways $ parse2 inp

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D06/input.txt"
  -- input <- readFile "src/Y23/D06/example.txt"
  print $ solve2 input
