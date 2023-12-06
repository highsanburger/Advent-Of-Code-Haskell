module Y23.D07.P1 where

import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Vector as V

solve :: String -> Int
solve inp = undefined

main :: IO ()
main = do
  -- input <- readFile "src/Y23/D07/input.txt"
  input <- readFile "src/Y23/D07/example.txt"
  print $ solve input
