module Y23.D05.P1 where

import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M

parse :: String -> [[String]]
parse inp = S.splitWhen (== "") $ lines inp

initial :: String -> [Int]
initial inp = map read $ tail $ words $ head . flip (!!) 0 $ parse inp

--  "x y z" -> [x,y,z]
str2Tup :: String -> [Int]
str2Tup x = map read $ words x

seed2soil :: String -> [[Int]]
seed2soil inp = map str2Tup $ tail $ flip (!!) 1 $ parse inp

soil2fert :: String -> [[Int]]
soil2fert inp = map str2Tup $ tail $ flip (!!) 2 $ parse inp

fert2watr :: String -> [[Int]]
fert2watr inp = map str2Tup $ tail $ flip (!!) 3 $ parse inp

watr2lite :: String -> [[Int]]
watr2lite inp = map str2Tup $ tail $ flip (!!) 4 $ parse inp

lite2temp :: String -> [[Int]]
lite2temp inp = map str2Tup $ tail $ flip (!!) 5 $ parse inp

temp2humi :: String -> [[Int]]
temp2humi inp = map str2Tup $ tail $ flip (!!) 6 $ parse inp

humi2locn :: String -> [[Int]]
humi2locn inp = map str2Tup $ tail $ flip (!!) 7 $ parse inp

corres :: Int -> [Int] -> Maybe Int
corres n range = case L.elemIndex n $ take (range !! 2) [(range !! 1) ..] of
  Nothing -> Nothing
  Just m -> Just $ [(head range) ..] !! m

ponding :: [[Int]] -> Int -> Int
ponding ranges n = case M.mapMaybe (corres n) ranges of
  [m] -> m
  [] -> n

solve :: String -> [Int]
solve input = map ((((((ponding (humi2locn input) . ponding (temp2humi input)) . ponding (lite2temp input)) . ponding (watr2lite input)) . ponding (fert2watr input)) . ponding (soil2fert input)) . ponding (seed2soil input)) (initial input)

-- why parsing for eve
main :: IO ()
main = do
  -- input <- readFile "src/Y23/D05/input.txt"
  input <- readFile "src/Y23/D05/example.txt"
  print $ solve input
