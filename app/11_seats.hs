module Main where

import System.Environment
import Text.Printf
import Data.Array
import Data.List (find)
import Data.List.Split (chunksOf)

type Seats = Array (Int, Int) Char

addPair :: Num a => (a,a) -> (a,a) -> (a,a)
addPair (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

directions = [(j,k) | j <- [-1,0,1], k <- [-1,0,1], j /= 0 || k /= 0]

scan :: Seats -> (Int, Int) -> (Int, Int) -> Bool
scan x (ix,iy) (dx, dy) =
    let
        scanline = takeWhile (inRange (bounds x)) [(ix + n * dx, iy + n * dy) | n <- [1..]]
    in
        case find (/= '.') . map (x!) $ scanline of
            Just '#' -> True
            _ -> False

occupiedNeighborsScan :: Seats -> (Int, Int) -> Int
occupiedNeighborsScan x i =
    length . filter id . map (scan x i) $ directions

updateSeatScan :: Seats -> (Int, Int) -> Char
updateSeatScan x i = case x ! i of
    '.' -> '.'
    'L' -> if occupiedNeighborsScan x i == 0 then '#' else 'L'
    '#' -> if occupiedNeighborsScan x i >= 5 then 'L' else '#'

occupiedNeighbors :: Seats -> (Int, Int) -> Int
occupiedNeighbors x i =
    length . filter (=='#') . map (x!) . filter (inRange (bounds x)) . map (addPair i) $ directions

updateSeat :: Seats -> (Int, Int) -> Char
updateSeat x i = case x ! i of
    '.' -> '.'
    'L' -> if occupiedNeighbors x i == 0 then '#' else 'L'
    '#' -> if occupiedNeighbors x i >= 4 then 'L' else '#'

updateSeats :: (Seats -> (Int, Int) -> Char) -> Seats -> Seats
updateSeats f x =
    let
        ix = indices x
        updated = map (f x) (indices x)
    in
        listArray (bounds x) updated

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
    if x == x' then x else fixpoint f x'
    where x' = f x

printSeats :: Int -> Seats -> IO ()
printSeats ny x =
    putStrLn $ unlines . chunksOf ny $ elems x

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)
    let
        nx = length l
        ny = length . head $ l
        m = listArray ((1, 1), (nx, ny)) (concat l)
        m' = fixpoint (updateSeats updateSeat) m
        ms = fixpoint (updateSeats updateSeatScan) m

    -- printSeats ny m'
    -- printSeats ny ms
    print $ length . filter (=='#') $ (elems m')
    print $ length . filter (=='#') $ (elems ms)
