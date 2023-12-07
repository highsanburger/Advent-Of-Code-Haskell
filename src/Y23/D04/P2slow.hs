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

a :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
a rec (id, numWin, numCards) =
  takeWhile (\(x, y, z) -> x <= id) rec
    ++ map (\(x, y, z) -> (x, y, z + numCards)) (takeWhile (\(x, y, z) -> x <= id + numWin) $ dropWhile (\(x, y, z) -> x <= id) rec)
    ++ dropWhile (\(x, y, z) -> x <= numWin + id) rec

rtj :: ([(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]) -> [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
rtj a r 0 = a r $ head r
rtj a r n = a (rtj a r (n - 1)) $ rtj a r (n - 1) !! n

sumRec :: [(Int, Int, Int)] -> Int
sumRec [] = 0
sumRec ((x, y, z) : rs) = z + sumRec rs

solve2 :: String -> Int
solve2 inp = sumRec $ rtj a rec (length rec - 1)
  where
    rec = map ((\(id, win, hav, num) -> (id, numWin win hav, num)) . parse2) (lines inp)

main2 :: IO ()
main2 = do
  -- input <- readFile "src/Y23/D04/input.txt"
  input <- readFile "src/Y23/D04/example.txt"
  print $ solve2 input
