module Main where

import System.Environment
import Text.Printf
import Data.List (span, transpose, find, delete, isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Set as S

newtype Idx3 = Idx3 (Int, Int, Int) deriving (Ord, Eq)
newtype Idx4 = Idx4 (Int, Int, Int, Int) deriving (Ord, Eq)

class (Ord i) => Idx i where
    combine :: (Int -> Int -> Int) -> i -> i -> i
    neighbors :: i -> [i]
    range :: i -> i -> [i]
    lo :: i
    hi :: i

instance Idx Idx3 where
    combine f (Idx3 (x, y, z)) (Idx3 (x', y', z'))
        = Idx3 (f x x', f y y', f z z')

    neighbors (Idx3 (x, y, z))
        = [Idx3 (x + i, y + j, z + k) | i <- [-1..1], j <- [-1..1], k <- [-1..1], i /= 0 || j /= 0 || k /= 0]

    range (Idx3 (lx, ly, lz)) (Idx3 (hx, hy, hz))
        = [Idx3 (i, j, k) | i <- [lx - 1 .. hx + 1], j <- [ly - 1 .. hy + 1], k <- [lz - 1 .. hz + 1]]

    lo = Idx3 (minBound, minBound, minBound)
    hi = Idx3 (maxBound, maxBound, maxBound)

instance Idx Idx4 where
    combine f (Idx4 (x, y, z, w)) (Idx4 (x', y', z', w'))
        = Idx4 (f x x', f y y', f z z', f w w')

    neighbors (Idx4 (x, y, z, w))
        = [Idx4 (x + i, y + j, z + k, w + l) | i <- [-1..1], j <- [-1..1], k <- [-1..1], l <- [-1..1], i /= 0 || j /= 0 || k /= 0 || l /= 0]

    range (Idx4 (lx, ly, lz, lw)) (Idx4 (hx, hy, hz, hw))
        = [Idx4 (i, j, k, l) | i <- [lx - 1 .. hx + 1], j <- [ly - 1 .. hy + 1], k <- [lz - 1 .. hz + 1], l <- [lw - 1 .. hw + 1]]

    lo = Idx4 (minBound, minBound, minBound, minBound)
    hi = Idx4 (maxBound, maxBound, maxBound, maxBound)

countIf p = length . filter p

countNeigbors :: (Idx i) => S.Set i -> i -> Int
countNeigbors state idx =
    countIf (flip S.member state) (neighbors idx)

isActive :: (Idx i) => S.Set i -> i -> Bool
isActive state idx =
    let
        n = countNeigbors state idx
    in
        n == 3 || (S.member idx state && n == 2)

runRound :: (Idx i) => S.Set i -> S.Set i
runRound state =
    let
        (l, h) = S.foldl (\(a, b) x -> (combine min a x, combine max b x)) (hi, lo) state
    in
        S.fromList $ filter (isActive state) (range l h)

buildSet :: (Idx i) => [(Char, i)] -> S.Set i
buildSet = S.fromList . map snd . filter ((=='#') . fst)

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)
    let
        rounds = 6
        nx = length l
        ny = length . head $ l

        initialState  = buildSet $ zip (concat l) [Idx3 (x,y,0)   | x <- [1..nx], y <- [1..ny]]
        initialState4 = buildSet $ zip (concat l) [Idx4 (x,y,0,0) | x <- [1..nx], y <- [1..ny]]

    print $ length $ (iterate runRound initialState) !! rounds
    print $ length $ (iterate runRound initialState4) !! rounds
