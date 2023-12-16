module Y23.D16.P2 where

import qualified Data.Char as C
import Y23.D16.P1

solve2 :: String -> Int
solve2 input = undefined

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D16/input.txt"
  -- input <- readFile "src/Y23/D16/example.txt"
  print $ solve2 input
