module Y23.D05.P1 where

import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Vector as V

initial :: String -> [Int]
initial inp = map read $ tail $ words $ head $ lines inp

-- parse :: String ->

-- solve :: String -> Int
-- solve inp = undefined

main :: IO ()
main = do
  -- input <- readFile "src/Y23/D05/input.txt"
  input <- readFile "src/Y23/D05/example.txt"
  print $ initial input

-- print $ solve input
