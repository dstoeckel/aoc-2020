module Main where

import System.Environment
import Text.Printf
import Data.Char
import Data.List.Split

countChar :: Char -> String -> Int
countChar c d = length $ filter (==c) d

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        entries = map (wordsBy (not . isAlphaNum)) (lines raw)
        entries' = map (\[a,b,c,d] -> (read a :: Int, read b :: Int, head c, d)) entries
        counts = map (\(a,b,c,d) -> (a, b, countChar c d)) entries'
        occurs = filter (\(a,b,c,d) -> (d !! (a - 1) == c) /= (d !! (b - 1) == c)) entries'

    print $ length $ filter (\(a,b,x) -> x >= a && x <= b) counts
    print $ length occurs
