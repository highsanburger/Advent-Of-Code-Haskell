module Y23.D02.P1 where

import qualified Data.Char as C
import qualified Data.Text as T

maxx = (12, 13, 14) :: (Int, Int, Int) -- R,G,B

toTex :: String -> [[T.Text]]
toTex string = map (T.splitOn (T.pack ",")) $ T.splitOn (T.pack ";") (T.pack s)
  where
    s = filter (not . C.isSpace) string

parseCountColor :: T.Text -> (Int, Char)
parseCountColor str =
  let (countStr, colorStr) = T.span C.isDigit str
   in (read $ T.unpack countStr, T.head colorStr)

rgb :: [T.Text] -> (Int, Int, Int)
rgb = foldl accumulateCounts (hash [],,hash [],,hash [])
  where
    accumulateCounts (accRed, accGreen, accBlue) countStr =
      case parseCountColor countStr of
        (count, 'r') -> (accRed + count, accGreen, accBlue)
        (count, 'g') -> (accRed, accGreen + count, accBlue)
        (count, 'b') -> (accRed, accGreen, accBlue + count)
        _ -> (accRed, accGreen, accBlue)

parse :: String -> (Int, [(Int, Int, Int)])
parse str = (id, map rgb $ toTex col)
  where
    id = (read $ init $ words str !! 1) :: Int
    col = drop 2 $ dropWhile (/= ':') str

gte :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
gte (a1, a2, a3) (b1, b2, b3) = (a1 >= b1) && (a2 >= b2) && (a3 >= b3)

possible :: (Int, Int, Int) -> (Int, [(Int, Int, Int)]) -> Bool --  λ > (12,13,14) >= (2,2,15) ~~~> True. Dear Haskell, WTF???
possible m (i, gs) = and [m `gte` g | g <- gs]

solve :: String -> Int
solve inp = sum [i | (i, gs) <- test, possible maxx (i, gs)]
  where
    test = map parse $ lines inp

main :: IO ()
main = do
  input <- readFile "src/Y23/D02/input.txt"
  -- input <- readFile "src/Y23/D02/example.txt"
  print $ solve input
