import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Text as T
import Data.Tuple

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

parse2 :: String -> [(Int,Int,Int)]
parse2 str =  map rgb $ toTex col
            where col = drop 2 $ dropWhile (/= ':') str

-- toArrs :: [(Int,Int,Int)] -> ([Int],[Int],[Int])
-- toArrs ((a,b,c):ts) = ([a],[b],[c])

max3 :: [(Int, Int, Int)] -> (Int, Int, Int)
max3 ts = (maximum $ map (\(x, _, _) -> x) ts,
               maximum $ map (\(_, y, _) -> y) ts,
               maximum $ map (\(_, _, z) -> z) ts)

power :: (Int,Int,Int) -> Int 
power (x,y,z) = x * y * z

main = do
    input <- readFile "02-input.txt"
    -- input <- readFile "example1.txt"
    print $ map max3 $ map parse2 $ lines $ input
    print $ map power $ map max3 $ map parse2 $ lines $ input
    print $ sum $ map power $ map max3 $ map parse2 $ lines $ input
