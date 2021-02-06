module Main where

import System.Environment
import Data.List
import Debug.Trace
import Data.Char (chr)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Direction = East | West | NorthEast | NorthWest | SouthEast | SouthWest

parseTilePath :: String -> [Direction]
parseTilePath [] = []
parseTilePath ('e':rs) = East : parseTilePath rs
parseTilePath ('w':rs) = West : parseTilePath rs
parseTilePath ('n':'e':rs) = NorthEast : parseTilePath rs
parseTilePath ('n':'w':rs) = NorthWest : parseTilePath rs
parseTilePath ('s':'e':rs) = SouthEast : parseTilePath rs
parseTilePath ('s':'w':rs) = SouthWest : parseTilePath rs
parseTilePath _ = undefined

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) East = (x, y + 1)
step (x, y) West = (x, y - 1)
step (x, y) NorthEast = (x - 1, if even x then y + 1 else y)
step (x, y) NorthWest = (x - 1, if even x then y else y - 1)
step (x, y) SouthEast = (x + 1, if even x then y + 1 else y)
step (x, y) SouthWest = (x + 1, if even x then y else y - 1)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors x = map (step x) [East, West, NorthEast, NorthWest, SouthEast, SouthWest]

alive :: S.Set (Int, Int) -> (Int, Int) -> Bool
alive tiles x =
    let
        count = length . filter (flip S.member tiles) . neighbors $ x
    in
        if S.member x tiles then count /= 0 && count <= 2 else count == 2

simulate :: S.Set (Int, Int) -> S.Set (Int, Int)
simulate x =
    let
        candidates = S.union x . S.fromList . concat . map neighbors . S.toList $ x
    in
        S.filter (alive x) candidates

main :: IO ()
main = do
    args <- getArgs
    paths <- map parseTilePath <$> lines <$> readFile (head args)

    let
        tiles = map (foldl step (0,0)) paths
        count = M.fromListWith (+) (zip tiles (repeat 1))
        live = S.fromList . M.keys . M.filter odd $ count

    -- Part 1
    print $ length live

    -- Part 2
    print $ length $ iterate simulate live !! 100
