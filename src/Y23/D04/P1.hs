module Y23.D04.P1 where

-- convert each line into -> (Card number, winning cards, the cards you have)
parse :: String -> (Int, [Int], [Int])
parse line = (cno, winning, have)
  where
    arr = tail $ words line
    cno = read $ init $ head arr
    winning = map read $ takeWhile (/= "|") $ tail arr :: [Int]
    have = map read $ tail $ dropWhile (/= "|") $ tail arr :: [Int]

elemOr :: [Bool] -> [Bool] -> [Bool]
elemOr [] [] = []
elemOr (b1 : b1s) (b2 : b2s) = (b1 || b2) : elemOr b1s b2s

-- function to see how many winning cards in have and where
winning = [41, 48, 83, 86, 17] :: [Int]

have = [83, 86, 6, 31, 17, 9, 48, 53] :: [Int]

mask :: [Int] -> Int -> [Bool]
mask have num = map (== num) have

maskAll :: [Int] -> [Int] -> [Bool]
maskAll winning have = foldr (elemOr . mask have) (replicate (length have) False) winning

points :: [Int] -> [Int] -> Int
points winning have = ps
  where
    bs = maskAll winning have
    len = length $ filter id bs
    ps = if len > 0 then 2 ^ (len - 1) else 0

sumPoints :: [(Int, Int)] -> Int
sumPoints ps = sum [p | (_, p) <- ps]

solve :: String -> Int
solve inp = sumPoints $ map ((\(id, win, hav) -> (id, points win hav)) . parse) (lines inp)

main :: IO ()
main = do
  input <- readFile "src/Y23/D04/input.txt"
  -- input <- readFile "src/Y23/D04/example.txt"
  print $ solve input
