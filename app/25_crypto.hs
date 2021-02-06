module Main where

import System.Environment
import Data.List
import Debug.Trace
import Data.Char (chr)
import qualified Data.Map.Strict as M

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

step :: Int -> Int -> Int
step sn x = mod (sn * x) 20201227

findLoopSize :: Int -> Int -> Maybe Int
findLoopSize sn pubKey = findIndex (==pubKey) . iterate (step sn) $ 1

main :: IO ()
main = do
    args <- getArgs
    [pubC, pubD] <- map read <$> lines <$> readFile (head args)

    let
        Just loopSizeC = findLoopSize 7 pubC
        Just loopSizeD = findLoopSize 7 pubD

        encC = iterate (step pubD) 1 !! loopSizeC
        encD = iterate (step pubC) 1 !! loopSizeD

    print $ loopSizeC
    print $ loopSizeD
    print encC
    print encD

