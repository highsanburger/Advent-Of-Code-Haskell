module Y23.D08.P2 where

import System.IO.Unsafe (unsafePerformIO)
import Y23.D08.P1

-- Define nwk as a global variable
nwk :: [[String]]
{-# NOINLINE nwk #-}
nwk = unsafePerformIO $ do
  fileContents <- readFile "src/Y23/D08/input.txt"
  return $ network fileContents

-- nwk = [[""]]

-- all docs with key _,_,A
starts :: [String]
starts = filter (\s -> last s == 'A') $ concat nwk

-- ["11A","22A"] -> Ins -> network ~~~> ["11B","22B"]
iter :: [String] -> String -> [String]
iter x ins = map (follow ins . findNwk nwk) x

ate :: [[String]] -> String -> [[String]]
ate [] ins = iter starts ins : ate [iter starts ins] (tail ins)
ate hist ins = if all (\x -> last x == 'Z') (last hist) then [] else iter (last hist) ins : ate (hist ++ [iter (last hist) ins]) (tail ins)

solve :: String -> [String]
solve input = full [] (cycle (instructions input)) (network input)

solve8 :: String -> [[String]]
solve8 input = ate [] (cycle (instructions input))

main28 :: IO ()
main28 = do
  input <- readFile "src/Y23/D08/example.txt"
  -- input <- readFile "src/Y23/D08/input.txt" -- print $ solve2 input
  print $ length $ solve8 input
