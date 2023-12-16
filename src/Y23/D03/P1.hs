module Y23.D03.P1 where

import qualified Data.Char as C
import qualified Data.Either as E

-- morph into something usable (Either Int Bool)
morph :: Char -> Either Int Bool
morph c
  | C.isDigit c = Left $ C.digitToInt c
  | c == '.' = Right False
  | otherwise = Right True

-- set of digits to set of nums
t = [Left 4, Left 6, Left 7, Right False, Right False, Left 1, Left 1, Left 4, Right False, Right False] :: [Either Int Bool]

decimalize :: [Either Int Bool] -> Int
decimalize es = foldr (\a b -> 10 * b + a) (hash []) ls
  where
    ls = reverse (E.lefts es)

-- replace each digit of number with whole number
digitToNumber :: [Either Int Bool] -> [Either Int Bool]
digitToNumber [] = []
digitToNumber (Right b : es) = Right b : digitToNumber es
digitToNumber (Left n : es) = map (const num) ns ++ digitToNumber bs
  where
    num = Left $ decimalize ns :: Either Int Bool
    ns = takeWhile E.isLeft (Left n : es)
    bs = dropWhile E.isLeft (Left n : es)

-- λ > t
-- [Left 4,Left 6,Left 7,Right False,Right False,Left 1,Left 1,Left 4,Right False,Right False]
--  λ > digitToNumber t
-- [Left 467,Left 467,Left 467,Right False,Right False,Left 114,Left 114,Left 114,Right False,Right False]

-- pad a Right False on all sides
pad :: [[Either Int Bool]] -> [[Either Int Bool]]
pad es = map (\x -> [nul] ++ x ++ [nul]) $ zs ++ es ++ zs
  where
    nul = Right False :: Either Int Bool
    zs = [replicate (length es) nul]

rowcol :: (Int, [(Int, Either Int Bool)]) -> [(Int, Int, Either Int Bool)]
rowcol (row, ces) = [(row, col, either) | (col, either) <- ces]

-- add pair of indices to all elements  [[(row,col,EIB)]]
indexify :: [[Either Int Bool]] -> [[(Int, Int, Either Int Bool)]]
indexify es = map rowcol m
  where
    m = zip [1 ..] $ map (zip [1 ..]) es :: [(Int, [(Int, Either Int Bool)])]

thd3 :: (Int, Int, Either Int Bool) -> Either Int Bool
thd3 (a, b, c) = c

-- get indices of the symbols
symbols :: [[(Int, Int, Either Int Bool)]] -> [(Int, Int)]
symbols es = map (\(r, c, e) -> (r, c)) $ filter (\x -> thd3 x == Right True) $ concat es

-- get indices around index
around :: (Int, Int) -> [(Int, Int)]
around (i, j) =
  [ (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
    (i, j - 1),
    (i, j + 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1)
  ]

validIndices :: [(Int, Int)] -> [(Int, Int)]
validIndices = concatMap around

first2 :: (Int, Int, Either Int Bool) -> (Int, Int)
first2 (a, b, c) = (a, b)

findElement :: [[(Int, Int, Either Int Bool)]] -> (Int, Int) -> Int
findElement es (x, y) = E.fromLeft (hash []) $ thd3 $ head $ filter (\a -> first2 a == (x, y)) $ concat es

rmdups :: [Int] -> [Int]
rmdups [] = []
rmdups (n : ns) = n : rmdups (dropWhile (== n) ns)

getValidInts :: [(Int, Int)] -> [[(Int, Int, Either Int Bool)]] -> [Int]
getValidInts vi es = rmdups $ map (findElement es) vi

solve :: String -> Int
solve ss = sum $ getValidInts vi ies
  where
    vi = validIndices $ symbols ies
    ies = indexify $ pad $ map (digitToNumber . map morph) $ lines ss

main :: IO ()
main = do
  input <- readFile "src/Y23/D03/input.txt"
  print $ solve input

-- WORKS BUT TWO NUMS SIDE BY SIDE BREAKS IT LOL
-- very slow too ~15s
