-- import qualified Data.Char as C
-- import qualified Data.Either as E
-- import qualified Data.Tuple as P

-- import Data.Maybe as M
-- import Data.List as L
-- import qualified Data.Text as T 

line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- convert each line into -> (Card number, winning cards, the cards you have)
parse :: String -> (Int,[Int],[Int])
parse line = (cno,winning,have)
              where arr = tail $ words $ line
                    cno = read $ init $ head arr
                    winning = map read $ takeWhile (/= "|") $ tail arr :: [Int]
                    have = map read $ tail $ dropWhile (/= "|") $ tail arr :: [Int]

elemOr :: [Bool] -> [Bool] -> [Bool]
elemOr [] [] = []
elemOr (b1:b1s) (b2:b2s) = ((b1 || b2) : elemOr b1s b2s)

-- function to see how many winning cards in have and where
winning = [41,48,83,86,17] :: [Int]
have = [83,86,6,31,17,9,48,53] :: [Int]

mask :: [Int] -> Int -> [Bool]
mask have num = map (\x -> x == num) have

maskAll :: [Int] -> [Int] -> [Bool]
maskAll winning have = foldr (\a b -> elemOr a b) (replicate (length have) False) (map (mask have) winning)


points :: [Int] -> [Int] -> Int
points winning have = ps 
                      where bs = maskAll winning have
                            len = (length $ filter (\b -> b) bs)
                            ps = if (len > 0) then 2 ^ (len - 1) else 0

sumPoints :: [(Int,Int)] -> Int
sumPoints ps = sum [ p | (_ , p) <- ps ]

main :: IO () 
main = do
  input <- readFile "example.txt"
  -- input <- readFile "input.txt"
  print $ map (\(id,win,hav) -> (id, points win hav)) $ map parse $ lines $ input
  -- print $ sumPoints $ map (\(id,win,hav) -> (id, points win hav)) $ map parse $ lines $ input
