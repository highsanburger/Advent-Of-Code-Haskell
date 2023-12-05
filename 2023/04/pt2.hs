line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- convert each line into -> (Card number, winning cards, the cards you have, # of instances)
parse :: String -> (Int,[Int],[Int],Int)
parse line = (cno,winning,have,1)
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

numWin :: [Int] -> [Int] -> Int
numWin winning have = (length $ filter (\b -> b) bs)
                      where bs = maskAll winning have


r = [(1,4,1),(2,2,1),(3,2,1),(4,1,1),(5,0,1),(6,0,1)] :: [(Int,Int,Int)]
a :: [(Int,Int,Int)] -> (Int,Int,Int) -> [(Int,Int,Int)]
a rec (id,numWin,numCards) =  (takeWhile (\(x,y,z) -> x <= id ) rec) ++
                                 (map (\(x,y,z) -> (x,y,z+numCards)) $ takeWhile (\(x,y,z) -> x <= id + numWin) $ dropWhile (\(x,y,z) -> x <= id) rec) ++
                                 (dropWhile (\(x,y,z) -> x <= numWin + id) rec)
rtj :: ( [(Int,Int,Int)] -> (Int,Int,Int) -> [(Int,Int,Int)] ) -> [(Int,Int,Int)] -> Int -> [(Int,Int,Int)]
rtj a r 0 = a r $ r!!0
rtj a r n = a (rtj a r (n-1)) $ (rtj a r (n-1)) !! n

sumRec :: [(Int,Int,Int)] -> Int
sumRec [] = 0
sumRec ((x,y,z):rs) = z + sumRec rs

main :: IO () 
main = do
  -- input <- readFile "example.txt"
  input <- readFile "input.txt"
  -- print $ map parse $ lines $ input
  let rec = map (\(id,win,hav,num) -> (id, numWin win hav,num)) $ map parse $ lines $ input
  print $ sumRec $ rtj a rec (length rec - 1)
