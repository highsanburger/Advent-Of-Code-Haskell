module Y23.D04.P2 where

import Y23.D04.P1

-- convert each line into -> (Card number, winning cards, the cards you have, # of instances)
parse2 :: String -> (Int, [Int], [Int], Int)
parse2 line = (cno, winning, have, 1)
  where
    arr = tail $ words line
    cno = read $ init $ head arr
    winning = map read $ takeWhile (/= "|") $ tail arr :: [Int]
    have = map read $ tail $ dropWhile (/= "|") $ tail arr :: [Int]

numWin :: [Int] -> [Int] -> Int
numWin winning have = length $ filter id bs
  where
    bs = maskAll winning have

update :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
update cs (id, win, num) = map (\(a, b, c) -> (a, b, c + num)) copies ++ rest
  where
    copies = filter (\(a, b, c) -> (a <= id + win) && (id < a)) cs
    rest = filter (\(a, b, c) -> a > id + win) cs

curse :: [(Int, Int, Int)] -> [(Int, Int, Int)]
curse [] = []
curse (c : cs) = c : curse (update cs c)

sumRec :: [(Int, Int, Int)] -> Int
sumRec [] = 0
sumRec ((x, y, z) : rs) = z + sumRec rs

solve2 :: String -> Int
solve2 inp = sumRec $ curse $ map ((\(id, win, hav, num) -> (id, numWin win hav, num)) . parse2) $ lines inp

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D04/input.txt"
  -- input <- readFile "src/Y23/D04/example.txt"
  print $ solve2 input
