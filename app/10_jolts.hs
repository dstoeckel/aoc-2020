module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)

import Data.List (sort)

arrangements [] = 1
arrangements (3:xs) = arrangements xs
arrangements (1:1:1:1:xs) = 7 * arrangements xs
arrangements (1:1:1:xs) = 4 * arrangements xs
arrangements (1:1:xs) = 2 * arrangements xs
arrangements (1:xs) = arrangements xs

main :: IO ()
main = do
    args <- getArgs
    code <- sort <$> map read <$> lines <$> readFile (head args) :: IO [Int]
    let
        device = 3 + maximum code
        differences = zipWith (-) (code ++ [device]) (0:code)
        n1 = length . filter (==1) $ differences
        n3 = length . filter (==3) $ differences

    print code
    print differences
    print n1
    print n3
    print $ n1 * n3

    print $ arrangements differences

