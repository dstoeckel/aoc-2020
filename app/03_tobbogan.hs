module Main where

import System.Environment
import Text.Printf
import Data.Array

type Slope = (Int, Int)
type TreeMap = Array (Int, Int) Char

countHits :: (Int, Int) -> TreeMap -> Slope -> Int
countHits (nx, ny) m (dx, dy) =
    let
        steps = div ny dy
        positions = [(mod (i * dx) nx, i * dy) | i <- [0 .. steps - 1]]
        hits = filter (\p -> m ! p == '#') positions
    in
        length hits

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
        l = lines raw
        nx = length . head $ l
        ny = length l
        m = array ((0, 0), (nx - 1, ny - 1)) (zip [(x,y) | y <- [0 .. ny - 1], x <- [0 .. nx - 1]] (concat l))
        hits = map (countHits (nx, ny) m) slopes
        output = map (\((dx, dy), h) -> "  " ++ show dx ++ ", " ++ show dy ++ " -> " ++ show h) (zip slopes hits)

    putStrLn "Number of trees hit:"
    putStrLn $ unlines output

    putStr "Product: "
    print $ foldl1 (*) hits
