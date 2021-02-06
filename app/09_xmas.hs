module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)

import Data.List (findIndex)

takeR :: Int -> [a] -> ([a], [a])
takeR i xs =
    let
        takeR' _ [] accum = (accum, [])
        takeR' 0 xs accum = (accum, xs)
        takeR' i (x:xs) accum = takeR' (i - 1) xs (x:accum)
    in
        takeR' i xs []

checkNumber [] n = False
checkNumber (x:xs) n = if any (\y -> x + y == n) xs then True else checkNumber xs n

findFirstMismatch :: [Int] -> [Int] -> Int -> Int
findFirstMismatch preamble (y:ys) n =
    let
        p = take n preamble
    in
        if checkNumber p y
        then findFirstMismatch (y:preamble) ys n
        else y


cumSum :: Num a => [a] -> [a]
cumSum xs =
    let
        cumSum' [] accum = []
        cumSum' (x:xs) accum = (x+accum) : cumSum' xs (x+accum)
    in
        cumSum' xs 0

sumMinMax :: (Ord a, Num a) => [a] -> a
sumMinMax xs = foldl1 max xs + foldl1 min xs

contiguousRange :: [Int] -> Int -> Int
contiguousRange xs n =
    case findIndex (==n) cs of
        Nothing -> contiguousRange (tail xs) n
        Just i -> sumMinMax (take i xs)
    where cs = cumSum xs

main :: IO ()
main = do
    args <- getArgs
    code <- map (read :: String -> Int) <$> lines <$> readFile (head args)
    let
        n = 25
        (preamble, rest) = takeR n code
        weakness = findFirstMismatch preamble rest n

    print weakness
    print $ contiguousRange code weakness

