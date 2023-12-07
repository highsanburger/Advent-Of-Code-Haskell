module Y23.D07.P2 where

import qualified Data.List as L
import Y23.D07.P1

isJ :: Hand -> Bool
isJ (Hand h) = J `elem` h

sortHs :: [Hand] -> [Hand]
sortHs = L.sortBy compareHand

cs = [N2, N3, N4, N5, N6, N7, N8, N9, T, J, Q, K, A]

possibleJs :: Hand -> [Card] -> [Hand]
possibleJs (Hand j) = map (\x -> Hand $ map (\c -> if c == J then x else c) j)

strongestJ :: Hand -> Hand
strongestJ h = L.maximumBy compareHand (possibleJs h cs)

makeJweak :: Hand -> Hand
makeJweak (Hand h) = Hand $ map (\c -> if c == J then NJ else c) h

mapJs :: [(Hand, Int)] -> [(Hand, Int, Hand)]
mapJs = map (\(h, b) -> (strongestJ h, b, makeJweak h))

compare2 :: (Hand, Int, Hand) -> (Hand, Int, Hand) -> Ordering
compare2 (Hand xj, n, Hand x) (Hand yj, m, Hand y) = case compare (giveTyype (Hand xj)) (giveTyype (Hand yj)) of
  EQ -> compare (Hand x) (Hand y)
  order -> order

sortHands2 :: [(Hand, Int, Hand)] -> [(Hand, Int, Hand)]
sortHands2 = L.sortBy compare2

solve2 :: String -> Int
solve2 inp = sum $ zipWith (curry (\(x, (y, z, k)) -> x * z)) [1 ..] (sortHands2 $ mapJs $ parse $ words inp)

main2 :: IO ()
main2 = do
  input <- readFile "src/Y23/D07/input.txt"
  -- input <- readFile "src/Y23/D07/example.txt"
  print $ solve2 input
