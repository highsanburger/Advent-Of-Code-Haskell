module Y23.D08.P1 where

import qualified Data.List as L

instructions :: String -> String
instructions input = head $ lines input

-- parse Node ["AAA","=","(BBB,","BBB)"] ~~~> ["AAA","BBB","CCC"]
parseNode :: [String] -> [String]
parseNode nd = [head nd, init . tail $ nd !! 2, init $ nd !! 3]

network :: String -> [[String]]
network input = map (parseNode . words) $ drop 2 $ lines input

findNwk :: [[String]] -> String -> [String]
findNwk nwk s = case L.elemIndex s $ map (\(a : as) -> a) nwk of
  Just n -> nwk !! n
  Nothing -> undefined

follow :: String -> [String] -> String
follow ('L' : insts) node = node !! 1
follow ('R' : insts) node = node !! 2

---     recurse -   traverse      same
--      history -- instructions -- network ->> history
full :: [String] -> String -> [[String]] -> [String]
full [] ins nwk = follow ins (findNwk nwk "AAA") : full [follow ins (findNwk nwk "AAA")] (tail ins) nwk
full ["ZZZ"] ins nwk = []
full history ins nwk = follow ins (findNwk nwk (last history)) : full [follow ins (findNwk nwk (last history))] (tail ins) nwk

-- data T a = L a | N (T a) a (T a) -- Tree/Leaf/Node
--
-- toTree :: [[String]] -> T String
-- toTree nwk = undefined

-- traverse :: String ->

solve :: String -> [String]
solve input = full [] (cycle (instructions input)) (network input)

-- cycle [1,2] ~~> [1,2,1,2,1,...]

main :: IO ()
main = do
  input <- readFile "src/Y23/D08/input.txt"
  -- input <- readFile "src/Y23/D08/example.txt"
  -- input <- readFile "src/Y23/D08/example2.txt"
  print $ solve input
  print $ length $ solve input
