module Main where

import System.Environment
import Data.List (foldl')
import qualified Data.IntMap.Strict as M
import Debug.Trace
import Data.Char (chr)

type Game = M.IntMap Int

makeInput :: String -> [Int]
makeInput = map (\x -> read [x])

initialize :: [Int] -> Game
initialize xs = M.fromList (zip xs (drop 1 . cycle $ xs))

computeDestination :: Int -> Int -> [Int] -> Int
computeDestination x m na =
    if x == 1 then computeDestination (m + 1) m na
    else if elem (x - 1) na then computeDestination (x - 1) m na
    else x - 1

collect :: Game -> [Int]
collect game = iterate (\i -> game M.! i) 1

step :: (Int, Game) -> (Int, Game)
step (current, game) =
    let
        maxElem = M.size game
        a = game M.! current
        b = game M.! a
        c = game M.! b
        next = game M.! c
        destination = computeDestination current maxElem [a, b, c]
        destinationNext = game M.! destination

    in
        (next, M.insert current next $ M.insert destination a $ M.insert c destinationNext game)

main :: IO ()
main = do
    let
        -- Example
        -- puzzle = "389125467"

        -- Puzzle Input
        puzzle = "394618527"

        (n, input) = (100, initialize . makeInput $ puzzle)
        (n2, input2) = (10000000, initialize $ makeInput puzzle ++ [10..1000000])

        -- Part 2
        [a, b] = take 2 . tail . collect . snd $ iterate step (3, input2) !! n2

    -- Part 1
    putStrLn $ map (chr . (+48)) . takeWhile (/=1) . tail . collect . snd $ iterate step (3, input) !! n
    print $ a * b
