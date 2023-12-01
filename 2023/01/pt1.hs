import Data.Maybe
-- import Data.Text as T
import Data.List 
-- removeAlpha :: Char -> Char
-- removeAlpha c = if ( elem c "0123456789" ) then c else '_'

toMaybeInt :: Char -> Maybe Int
toMaybeInt '0' = Just 0
toMaybeInt '1' = Just 1
toMaybeInt '2' = Just 2
toMaybeInt '3' = Just 3
toMaybeInt '4' = Just 4
toMaybeInt '5' = Just 5
toMaybeInt '6' = Just 6
toMaybeInt '7' = Just 7
toMaybeInt '8' = Just 8
toMaybeInt '9' = Just 9
toMaybeInt _ = Nothing


toNum :: [Int] -> Int
-- toNum ns = sum $ map (\(t,d) -> d*(10^t)) $ zip [0..] $ reverse ns
toNum ns = (ns !! 0) * 10 + (ns !! (length ns -1))

toMaybeIntString :: String -> Int 
toMaybeIntString str = toNum $ catMaybes $ map toMaybeInt str

main = do
    -- input <- readFile "01-input.txt"
    -- input <- readFile "example1.txt"
    input <- readFile "example2.txt"
    -- pt one
    -- print $ lines $ input
    -- print $ map toMaybeIntString $ lines $ input
    -- print $ sum $ map toMaybeIntString $ lines $ input
    -- pt two
    print $ map (extractWords numWords) $ lines $ input
    

