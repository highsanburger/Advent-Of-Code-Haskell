module Y23.D10.P1 where

-- import qualified Data.Matrix as X

-- import qualified Data.Char as C
-- import qualified Data.Either as E
-- import qualified Data.List as L
-- import qualified Data.Maybe as M
-- import qualified Data.Text as T
-- import qualified Data.Vector as V

-- data Pipe = V | H | NE | NW | SW | SE | G | S deriving (Show, Eq)
--
-- -- type Tiles = X.Matrix Pipe
--
-- toPipe :: Char -> Pipe
-- toPipe '|' = V
-- toPipe '-' = H
-- toPipe 'L' = NE
-- toPipe 'J' = NW
-- toPipe '7' = SW
-- toPipe 'F' = SE
-- toPipe '.' = G
-- toPipe 'S' = S
-- toPipe _ = undefined
--
-- parse :: String -> Tiles
-- parse input = X.fromLists $ map (map toPipe) $ lines input
--
-- padMat :: Tiles -> Tiles
-- padMat tile = pc X.<-> (pr X.<|> tile X.<|> pr) X.<-> pc
--   where
--     pr = X.fromList (X.nrows tile) 1 (repeat G)
--     pc = X.fromList 1 (X.ncols tile + 2) (repeat G)
--
-- -- adjacent :: Pipe -> [Pipe]
--
-- solve :: String -> Int
-- solve input = undefined
--
-- main :: IO ()
-- main = do
--   -- input <- readFile "src/Y23/D10/input.txt"
--   input <- readFile "src/Y23/D10/loop1.txt"
--   print $ solve input
