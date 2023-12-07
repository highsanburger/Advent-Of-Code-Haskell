module Y23.D06.P1 where

parse :: String -> [(Int, Int)]
parse input = zip time dist
  where
    time = map read $ tail $ words $ takeWhile (/= '\n') input
    dist = map read $ tail $ words $ dropWhile (/= '\n') input

allTimes :: (Int, Int) -> [(Int, Int)]
allTimes (t, _) = [(x, x * (t - x)) | x <- [0 .. t]]

isWinning :: (Int, Int) -> (Int, Int) -> Bool
isWinning (t, d) (t', d') = d' > d

ways :: (Int, Int) -> Int
ways (t, d) = length $ filter (isWinning (t, d)) $ allTimes (t, d)

solve :: String -> Int
solve inp = product $ map ways $ parse inp

main :: IO ()
main = do
  input <- readFile "src/Y23/D06/input.txt"
  -- input <- readFile "src/Y23/D06/example.txt"
  print $ solve input

-- 30 seconds meh
