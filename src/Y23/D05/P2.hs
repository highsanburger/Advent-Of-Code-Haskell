module Y23.D05.P2 where

import qualified Data.IntMap.Strict as M
import qualified Data.List.Split as S
import qualified Data.Vector as V

main :: IO ()
main = do
  let result = initRange [1, 5, 3, 10, 2, 7]
  print result

parse :: String -> [[String]]
parse input = S.splitWhen (== "") $ lines input

initial :: String -> [Int]
initial input = map read $ tail $ words $ head . flip (!!) 0 $ S.splitWhen (== "") $ lines input

initRange :: [Int] -> V.Vector Int
initRange [] = V.empty
initRange (start : len : irs) = V.enumFromN start len V.++ initRange irs
initRange [_] = error "Invalid input"

parseNums :: [String] -> [[Int]]
parseNums ss = map (map read) $ tail $ map words ss

ranges :: [Int] -> [(Int, Int)]
ranges [destination, source, len] = zip (take len [source ..]) [destination ..]
ranges _ = undefined

parseOnce :: String -> [[[Int]]]
parseOnce input = map (map (map read) . tail . map words) (parse input)

seedSoil :: [[[Int]]] -> M.IntMap Int
seedSoil m = M.fromList $ concatMap ranges (m !! 1)

soilFert :: [[[Int]]] -> M.IntMap Int
soilFert m = M.fromList $ concatMap ranges (m !! 2)

fertWatr :: [[[Int]]] -> M.IntMap Int
fertWatr m = M.fromList $ concatMap ranges (m !! 3)

watrLite :: [[[Int]]] -> M.IntMap Int
watrLite m = M.fromList $ concatMap ranges (m !! 4)

liteTemp :: [[[Int]]] -> M.IntMap Int
liteTemp m = M.fromList $ concatMap ranges (m !! 5)

tempHumi :: [[[Int]]] -> M.IntMap Int
tempHumi m = M.fromList $ concatMap ranges (m !! 6)

humiLocn :: [[[Int]]] -> M.IntMap Int
humiLocn m = M.fromList $ concatMap ranges (m !! 7)

tally :: M.IntMap Int -> M.Key -> Int
tally mapp key = M.findWithDefault key key mapp

corresponds :: [[[Int]]] -> M.Key -> Int
corresponds input = tally (humiLocn input) . tally (tempHumi input) . tally (liteTemp input) . tally (watrLite input) . tally (fertWatr input) . tally (soilFert input) . tally (seedSoil input)

-- main2 :: IO ()
-- main2 = do
-- input <- readFile "src/Y23/D05/input.txt"
-- input <- readFile "src/Y23/D05/example.txt"
-- let m = parseOnce input
-- let ini = initRange $ initial input
-- print $ corresponds m (ini !! 0)

--
-- print $ minimum $ map (corresponds m) ini
