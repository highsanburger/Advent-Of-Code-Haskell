import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Text as T

maxx = (12,13,14) :: (Int,Int,Int) -- R,G,B

toTex :: String -> [[T.Text]]
toTex string = map ( T.splitOn (T.pack ",") )$ T.splitOn (T.pack ";") (T.pack s)
                where s = filter (\x -> not $ isSpace x) string

rgb :: [T.Text] -> (Int, Int, Int)
rgb = foldl accumulateCounts (0, 0, 0)
  where
    accumulateCounts (accRed, accGreen, accBlue) countStr =
      case parseCountColor countStr of
        (count, 'r') -> (accRed + count, accGreen, accBlue)
        (count, 'g') -> (accRed, accGreen + count, accBlue)
        (count, 'b') -> (accRed, accGreen, accBlue + count)
        _            -> (accRed, accGreen, accBlue)

parseCountColor :: T.Text -> (Int, Char)
parseCountColor str =
  let (countStr, colorStr) = T.span isDigit str
  in (read $ T.unpack countStr, T.head colorStr)

parse :: String -> (Int,[(Int,Int,Int)])
parse str =  (id,map rgb $ toTex col)
            where id = (read $ init $ words str !! 1) :: (Int)
                  col = drop 2 $ dropWhile (/= ':') str

gte :: (Int, Int, Int) -> (Int, Int, Int) -> Bool 
gte (a1,a2,a3) (b1,b2,b3) = (a1 >= b1) &&  (a2 >= b2) &&  (a3 >= b3)

possible :: (Int,Int,Int) -> (Int,[(Int,Int,Int)]) -> Bool  --  λ > (12,13,14) >= (2,2,15) ~~~> True WTF???
possible m (i,gs) = and [m `gte` g | g <- gs]

str =  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
s = "Game 76: 5 green, 13 red, 10 blue; 5 red, 11 green; 1 red, 5 green, 8 blue; 4 red, 14 green; 7 blue, 12 green, 2 red"
col = "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

test = [(1,[(4,0,3),(1,2,6),(0,2,0)]),(2,[(0,2,1),(1,3,4),(0,1,1)]),(3,[(20,8,6),(4,13,5),(1,5,0)]),(4,[(3,1,6),(6,3,0),(14,3,15)]),(5,[(6,3,1),(1,2,2)])] :: [(Int,[(Int,Int,Int)])]

main = do
    input <- readFile "02-input.txt" -- < 3115 , > 1935
    -- input <- readFile "example1.txt"
    let test = map parse $ lines $ input
    print $ [i | (i, gs) <- test, possible maxx (i, gs)]
    print $ sum [i | (i, gs) <- test,  possible maxx (i, gs)]
