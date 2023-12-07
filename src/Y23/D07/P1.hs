module Y23.D07.P1 where

import qualified Data.List as L

data Card = NJ | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving (Show, Eq, Ord)

newtype Hand = Hand [Card] deriving (Show, Eq, Ord)

toCard :: Char -> Card
toCard '2' = N2
toCard '3' = N3
toCard '4' = N4
toCard '5' = N5
toCard '6' = N6
toCard '7' = N7
toCard '8' = N8
toCard '9' = N9
toCard 'T' = T
toCard 'J' = J
toCard 'Q' = Q
toCard 'K' = K
toCard 'A' = A

s = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\n QQQJA 483"

parse :: [String] -> [(Hand, Int)]
parse [] = []
parse (hw : bw : ss) = (Hand $ map toCard hw, read bw) : parse ss

hlen :: Hand -> [Int]
hlen (Hand cs) = map length $ L.group $ L.sort cs

isFive :: Hand -> Bool
isFive h = elem 5 $ hlen h

isFour :: Hand -> Bool
isFour h = elem 4 (hlen h) && not (isFive h)

isFull :: Hand -> Bool
isFull h = elem 3 (hlen h) && elem 2 (hlen h)

isThree :: Hand -> Bool
isThree h = (3 `elem` hlen h) && not (isFive h) && not (isFour h) && not (isFull h)

isTwoP :: Hand -> Bool
isTwoP h = L.isInfixOf [2, 2] $ L.sort $ hlen h

isOneP :: Hand -> Bool
isOneP h = (2 `elem` hlen h) && not (isFive h) && not (isFour h) && not (isFull h) && not (isThree h) && not (isTwoP h)

isHigh :: Hand -> Bool
isHigh h = not (isFive h || isFour h || isFull h || isThree h || isTwoP h || isOneP h)

data Tyype = High | OnePair | TwoPair | Three | Full | Four | Five deriving (Show, Eq, Ord)

giveTyype :: Hand -> Tyype
giveTyype h
  | isHigh h = High
  | isOneP h = OnePair
  | isTwoP h = TwoPair
  | isThree h = Three
  | isFull h = Full
  | isFour h = Four
  | isFive h = Five

compareHand :: Hand -> Hand -> Ordering
compareHand (Hand g) (Hand h) = if giveTyype (Hand g) == giveTyype (Hand h) then compare g h else compare (giveTyype (Hand g)) (giveTyype (Hand h))

sortHands :: [(Hand, Int)] -> [(Hand, Int)]
sortHands = L.sortBy (\(g, _) (h, _) -> compareHand g h)

solve :: String -> Int
solve inp = sum $ zipWith (curry (\(x, (y, z)) -> x * z)) [1 ..] (sortHands $ parse $ words inp)

main :: IO ()
main = do
  input <- readFile "src/Y23/D07/input.txt"
  -- input <- readFile "src/Y23/D07/example.txt"
  print $ solve input
