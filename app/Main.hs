{-# LANGUAGE BangPatterns #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

parse :: T.Text -> (Int, V.Vector Int, V.Vector Int, Int)
parse line = (cno, V.fromList winning, V.fromList have, 1)
  where
    parts = tail $ T.words line
    cno = read $ T.unpack $ T.init $ head parts
    winning = map read $ takeWhile (/= "|") $ map T.unpack $ tail parts
    have = map read $ tail $ dropWhile (/= "|") $ map T.unpack $ tail parts

mask :: V.Vector Int -> Int -> V.Vector Bool
mask have num = V.map (== num) have

maskAll :: V.Vector Int -> V.Vector Int -> V.Vector Bool
maskAll winning have = V.foldr (V.zipWith (||)) (V.replicate (V.length have) False) masks
  where
    masks = V.map (\num -> mask have num) winning

numWin :: V.Vector Int -> V.Vector Int -> Int
numWin winning have = V.length $ V.filter id $ maskAll winning have

updateRecord :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
updateRecord rec (!id, !numWin, !numCards) =
  takeWhile (\(x, _, _) -> x <= id) rec ++
  map (\(x, y, z) -> (x, y, z + numCards)) (takeWhile (\(x, _, _) -> x <= id + numWin) (dropWhile (\(x, _, _) -> x <= id) rec)) ++
  dropWhile (\(x, _, _) -> x <= numWin + id) rec

recursiveUpdate :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
recursiveUpdate rec 0 = updateRecord rec $ head rec
recursiveUpdate rec n = updateRecord (recursiveUpdate rec (n - 1)) (recursiveUpdate rec (n - 1) !! n)

sumRec :: [(Int, Int, Int)] -> Int
sumRec = foldr (\(_, _, z) acc -> z + acc) 0

main :: IO ()
main = do
  input <- TIO.readFile "input.txt"
  let rec = map (\(id, win, hav, num) -> (id, numWin win hav, num)) $ map parse (T.lines input)
  print $ sumRec $ recursiveUpdate rec (length rec - 1)

