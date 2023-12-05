import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Tuple as P

-- import Data.Maybe as M
import Data.List as L
-- import qualified Data.Text as T 

-- morph into sm usable (Either Int Bool)
morph :: Char -> Either Int Bool
morph c | C.isDigit c = Left $ C.digitToInt c
        | c == '.'    = Right False
        | otherwise   = Right True

-- set of digits to set of nums
t = [Left 4,Left 6,Left 7,Right False,Right False,Left 1,Left 1,Left 4,Right False,Right False] :: [Either Int Bool]

decimalize :: [Either Int Bool] -> Int 
decimalize es = foldr (\a b -> 10 * b + a) 0 ls
                where ls = E.lefts $ reverse es

-- replace each digit of number with whole number
digitToNumber :: [Either Int Bool] -> [Either Int Bool]
digitToNumber [] = [] 
digitToNumber (Right b : es) = (Right b : digitToNumber es)
digitToNumber (Left n : es)  = map (\x -> num) ns ++ digitToNumber bs
                                where num = Left $ decimalize ns :: Either Int Bool
                                      ns = takeWhile ( E.isLeft ) (Left n : es) 
                                      bs = dropWhile ( E.isLeft ) (Left n : es) 
-- λ > t
-- [Left 4,Left 6,Left 7,Right False,Right False,Left 1,Left 1,Left 4,Right False,Right False]
--  λ > digitToNumber t
-- [Left 467,Left 467,Left 467,Right False,Right False,Left 114,Left 114,Left 114,Right False,Right False]

-- pad a Right False on all sides
pad :: [[Either Int Bool]] -> [[Either Int Bool]]
pad es = map (\x ->[nul] ++ x ++ [nul]) $ zs ++ es ++ zs
            where nul = Right False :: Either Int Bool 
                  zs = [replicate (length es) nul]

es = [[Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False],[Right False,Left 467,Left 467,Left 467,Right False,Right False,Left 114,Left 114,Left 114,Right False,Right False,Right False],[Right False,Right False,Right False,Right False,Right True,Right False,Right False,Right False,Right False,Right False,Right False,Right False],[Right False,Right False,Right False,Left 35,Left 35,Right False,Right False,Left 633,Left 633,Left 633,Right False,Right False],[Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right True,Right False,Right False,Right False,Right False],[Right False,Left 617,Left 617,Left 617,Right True,Right False,Right False,Right False,Right False,Right False,Right False,Right False],[Right False,Right False,Right False,Right False,Right False,Right False,Right True,Right False,Left 58,Left 58,Right False,Right False],[Right False,Right False,Right False,Left 592,Left 592,Left 592,Right False,Right False,Right False,Right False,Right False,Right False],[Right False,Right False,Right False,Right False,Right False,Right False,Right False,Left 755,Left 755,Left 755,Right False,Right False],[Right False,Right False,Right False,Right False,Right True,Right False,Right True,Right False,Right False,Right False,Right False,Right False],[Right False,Right False,Left 664,Left 664,Left 664,Right False,Left 598,Left 598,Left 598,Right False,Right False,Right False],[Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False,Right False]] :: [[Either Int Bool]]

rowcol :: (Int,[(Int, Either Int Bool)]) -> [(Int,Int,Either Int Bool)]
rowcol (row,ces) = [ (row,col,either)| (col,either) <- ces]

-- add pair of indices to all elements  [[(row,col,EIB)]]
indexify :: [[Either Int Bool]] -> [[(Int,Int,Either Int Bool)]]
indexify es = map rowcol m 
                where m = zip [1..] $ map (zip [1..]) es :: [(Int,[(Int,Either Int Bool)])]

thd3 :: (Int, Int, Either Int Bool) -> Either Int Bool
thd3 (a,b,c) = c

-- map over 
-- for (i,j) the 8 elements around it are :- 

-- get indices of the symbols 
symbols :: [[(Int, Int, Either Int Bool)]] -> [(Int, Int)]
symbols es = map (\(r,c,e) -> (r,c) ) $ filter (\x -> thd3 x == Right True) $ concat es

-- get indices around index
around :: (Int, Int) -> [(Int, Int)]
around (i,j) = [
    (i-1, j-1), (i-1,j), (i-1,j+1) ,
    (i, j-1),  (i,j+1),
    (i+1, j-1), (i+1,j), (i+1,j+1)
    ]


validIndices :: [(Int,Int)] -> [(Int,Int)]
validIndices = concat . map around

first2 :: (Int, Int, Either Int Bool) -> (Int,Int)
first2 (a,b,c) = (a,b)

findElement ::  [[(Int, Int, Either Int Bool)]] -> (Int,Int) -> Int
findElement es (x,y) =  E.fromLeft 0 $ thd3 $ head $ filter (\a -> first2 a == (x,y)) $ concat es

rmdups :: [Int] -> [Int]
rmdups [] = []
rmdups (n:ns) = [n] ++ (rmdups $ dropWhile (\x -> x == n) ns)

getValidInts :: [(Int,Int)] -> [[(Int, Int, Either Int Bool)]] -> [Int]
getValidInts vi es = rmdups $ map (findElement es) vi                       

final :: [String] -> [Int]
final ss = getValidInts (vi) (ies) 
        where vi = validIndices $ symbols $ ies
              ies = indexify $ pad $ map (digitToNumber . (map morph)) ss

{- PART 2-}

ex = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

iex = indexify $ pad $ map (digitToNumber . map morph) ex
sex = symbols iex

getAdjacent :: [String] -> [[Int]]
getAdjacent ex = map (map (\x -> findElement iex x)) $ map around sex
                where iex = indexify $ pad $ map (digitToNumber . map morph) ex
                      sex = symbols iex

final2 :: [String] -> Int
final2 ex = sum $ map product $ filter (\arr -> length arr == 2) $ map (filter (/= 0)) $ map rmdups $ getAdjacent ex

validIndices2 :: [(Int,Int)] -> [((Int,Int),(Int,Int))]
validIndices2 = undefined


-- gearRatios ::[(Int,Int)] -> [[(Int, Int, Either Int Bool)]] -> [(Int,Int)] 
-- gearRatios vi es = 

main :: IO ()
main = do
    input <- readFile "input.txt"
    -- input <- readFile "example1.txt"
    -- print $ pad $ map (digitToNumber . (map morph)) $ lines $ input
    -- print $ final $ lines $ input
    -- print $ sum $ final $ lines $ input
    -- print $ lines $ input
    print $ final2 $ lines $ input


    {-
     - WORKS BUT TWO NUMS SIDE BY SIDE BREAKS IT LOL
     - -}

