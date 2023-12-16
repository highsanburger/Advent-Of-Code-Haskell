module Y23.D15.P2 where

import qualified Data.Char as C
import Y23.D15.P1 (hash, parse)

operation :: String -> Maybe Int
operation "-" = Nothing
operation s = Just (read $ tail s)

parse2 :: String -> [(String, Maybe Int)]
parse2 input = map (\s -> (takeWhile C.isAlpha s, operation $ dropWhile C.isAlpha s)) $ parse input

boxes :: [(Int, [(String, Int)])]
boxes = zip [0 ..] $ replicate 256 []

operate :: [(String, Int)] -> (String, Maybe Int) -> [(String, Int)]
operate box (label, mi) = case mi of
  Nothing -> filter (\x -> fst x /= label) box
  Just n -> if label `elem` map fst box then map (\b -> if fst b == label then (label, n) else b) box else (label, n) : box

hashmap :: [(Int, [(String, Int)])] -> [(String, Maybe Int)] -> [(Int, [(String, Int)])]
hashmap boxs [] = boxs
hashmap boxs (l : ls) = hashmap (map (\b -> if fst b == h then (h, operate (snd b) l) else b) boxs) ls
  where
    h = hash $ fst l

power :: (Int, [(String, Int)]) -> [Int]
power (bno, bs) = foldr (\(slot, (_, focal)) acc -> (bno + 1) * slot * focal : acc) [] (zip [1 ..] $ reverse bs)

solve2 :: String -> Int
solve2 input = sum $ concatMap power (hashmap boxes (parse2 input))

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D15/input.txt"
  -- input <- readFile "src/Y23/D15/example.txt"
  print $ solve2 input
