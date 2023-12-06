module Y23.D01.P1 where

import qualified Data.Maybe as M

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
toNum ns = head ns * 10 + (ns !! (length ns - 1))

toMaybeIntString :: String -> Int
toMaybeIntString str = toNum $ M.mapMaybe toMaybeInt str

solve :: String -> Int
solve inp = sum $ map toMaybeIntString $ lines inp

main :: IO ()
main = do
  input <- readFile "src/Y23/D01/input.txt"
  print $ solve input
