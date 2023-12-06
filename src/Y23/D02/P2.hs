module Y23.D02.P2 where

import Y23.D02.P1

parse2 :: String -> [(Int, Int, Int)]
parse2 str = map rgb $ toTex col
  where
    col = drop 2 $ dropWhile (/= ':') str

max3 :: [(Int, Int, Int)] -> (Int, Int, Int)
max3 ts =
  ( maximum $ map (\(x, _, _) -> x) ts,
    maximum $ map (\(_, y, _) -> y) ts,
    maximum $ map (\(_, _, z) -> z) ts
  )

power :: (Int, Int, Int) -> Int
power (x, y, z) = x * y * z

solve2 :: String -> Int
solve2 inp = sum $ map ((power . max3) . parse2) (lines inp)

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D02/input.txt"
  print $ solve2 input
