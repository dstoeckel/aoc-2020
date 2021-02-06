module Main where

import System.Environment
import Text.Printf
import Data.Char
import Data.Bits (shift, shiftR, (.&.))
import Data.List (sort, find)

toBinary :: Char -> Int
toBinary 'F' = 0
toBinary 'B' = 1
toBinary 'L' = 0
toBinary 'R' = 1

positionToNumber (x, i) = shift (toBinary x) i

parseBoardingPass :: String -> Int
parseBoardingPass p =
    let
        p' = zip (reverse p) [0..]
    in
        sum $ map positionToNumber p'

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        l = lines raw
        passes = sort $ map parseBoardingPass l

    putStrLn $ "Minimum seat id: " ++ show (foldl1 min passes)
    putStrLn $ "Maximum seat id: " ++ show (foldl1 max passes)

    let
        diffs = map (\(x,y) -> x - y) (zip (tail passes) passes)
        Just (firstSeat, _) = find (\(p,d) -> d > 1) (zip passes diffs)

    putStrLn $ "First empty seat: " ++ show (firstSeat + 1)
