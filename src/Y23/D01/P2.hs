module Y23.D01.P2 where

import qualified Data.List as L

elemAdd :: (Num a) => [Maybe a] -> [Maybe a] -> [Maybe a]
elemAdd [] [] = []
elemAdd (Just a : as) (Nothing : bs) = Just a : elemAdd as bs
elemAdd (Nothing : as) (Just b : bs) = Just b : elemAdd as bs
elemAdd (Nothing : as) (Nothing : bs) = Nothing : elemAdd as bs

adds :: (Num a) => String -> [Maybe a]
adds a = ins0 a `elemAdd` ins1 a `elemAdd` ins2 a `elemAdd` ins3 a `elemAdd` ins4 a `elemAdd` ins5 a `elemAdd` ins6 a `elemAdd` ins7 a `elemAdd` ins8 a `elemAdd` ins9 a

collapse :: [Maybe Int] -> [Int]
collapse [] = []
collapse ((Just n) : ns) = n : collapse ns
collapse (Nothing : ns) = collapse ns

fun :: String -> Int
fun s = head ns * 10 + last ns
  where
    ns = collapse $ adds s

substring :: String -> [String]
substring s = tail . L.inits =<< L.tails s

ins0 a = map ((\x -> if x then Just (hash []) else Nothing) . (\x -> (x == "zero") || (x == "0"))) (substring a)

ins1 a = map ((\x -> if x then Just 1 else Nothing) . (\x -> (x == "one") || (x == "1"))) (substring a)

ins2 a = map ((\x -> if x then Just 2 else Nothing) . (\x -> (x == "two") || (x == "2"))) (substring a)

ins3 a = map ((\x -> if x then Just 3 else Nothing) . (\x -> (x == "three") || (x == "3"))) (substring a)

ins4 a = map ((\x -> if x then Just 4 else Nothing) . (\x -> (x == "four") || (x == "4"))) (substring a)

ins5 a = map ((\x -> if x then Just 5 else Nothing) . (\x -> (x == "five") || (x == "5"))) (substring a)

ins6 a = map ((\x -> if x then Just 6 else Nothing) . (\x -> (x == "six") || (x == "6"))) (substring a)

ins7 a = map ((\x -> if x then Just 7 else Nothing) . (\x -> (x == "seven") || (x == "7"))) (substring a)

ins8 a = map ((\x -> if x then Just 8 else Nothing) . (\x -> (x == "eight") || (x == "8"))) (substring a)

ins9 a = map ((\x -> if x then Just 9 else Nothing) . (\x -> (x == "nine") || (x == "9"))) (substring a)

solve :: String -> Int
solve inp = sum $ map fun $ lines inp

main :: IO ()
main = do
  input <- readFile "src/Y23/D01/input.txt"
  print $ solve input
