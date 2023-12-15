module Y23.D05.P2 where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M

parse :: String -> [[String]]
parse inp = S.splitWhen (== "") $ lines inp

initial :: String -> [Int]
initial inp = map read $ tail $ words $ head . flip (!!) 0 $ parse inp

-- part2 modification
initial2 :: [Int] -> [Int]
initial2 [_] = undefined
initial2 [] = []
initial2 (start : len : ns) = kys ++ initial2 ns
  where
    kys = take len [start ..]

str2Tup :: String -> [Int]
str2Tup x = map read $ words x

seed2soil :: String -> [[Int]]
seed2soil inp = map str2Tup $ tail $ flip (!!) 1 $ parse inp

-- Define ponding with precomputed results
ponding :: A.Array Int Int -> Int -> Int
ponding results n = results A.! n

computePondingResults :: A.Array Int Int -> [[Int]] -> A.Array Int Int
computePondingResults results ranges =
  A.array (0, length results - 1) $
    zip [0 ..] $
      map
        ( \n -> case M.mapMaybe (corres n) ranges of
            [m] -> m
            [] -> n
        )
        (A.elems results)

corres :: Int -> [Int] -> Maybe Int
corres n range =
  case L.elemIndex n $ take (range !! 2) [(range !! 1) ..] of
    Nothing -> Nothing
    Just m -> Just $ [(head range) ..] !! m

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

m :: String -> [[[Int]]]
m input =
  [ seed2soil input,
    soil2fert input,
    fert2watr input,
    watr2lite input,
    lite2temp input,
    temp2humi input,
    humi2locn input
  ]

solve5 :: [Int] -> [[[Int]]] -> [Int]
solve5 ini mat =
  let initialResults = A.listArray (0, length ini - 1) ini
      finalResults = foldl computePondingResults initialResults mat
   in A.elems finalResults

main25 :: IO ()
main25 = do
  -- input <- readFile "src/Y23/D05/input.txt"
  input <- readFile "src/Y23/D05/example.txt"
  let ini = initial2 $ initial input
  let mat = m input
  print $ solve5 ini mat
  print $ minimum $ solve ini mat

-- LITERALLY HOURS (2-3)
-- 👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎👎
