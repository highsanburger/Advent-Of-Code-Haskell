module Y23.D09.P1 where

parse :: String -> [[Int]]
parse input = map (map read . words) (lines input) :: [[Int]]

diff :: [Int] -> [Int]
diff [_] = []
diff (x : y : ns) = (y - x) : diff (y : ns)
diff [] = undefined

seqs :: [Int] -> [[Int]]
seqs ns = if all (== 0) (diff ns) then [ns, diff ns] else ns : seqs (diff ns)

extapolate :: [[Int]] -> Int
extapolate = foldr (\a b -> b + last a) 0

solve :: String -> [Int]
solve input = map (extapolate . seqs) (parse input)

add x = x + 1

main :: IO ()
main = do
  input <- readFile "src/Y23/D09/input.txt"
  -- input <- readFile "src/Y23/D09/example.txt"
  print $ solve input
  print $ sum $ solve input
