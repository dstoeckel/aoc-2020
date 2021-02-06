module Main where

import System.Environment
import Text.Printf

combination x ys = foldr ((:) . (,) x) [] ys

pairs [] = []
pairs (x:xs) = combination x xs ++ pairs xs

triples [] = []
triples (x:xs) = combination x (pairs xs) ++ triples xs

main :: IO ()
main = do
    args <- getArgs
    l <- map read <$> lines <$> readFile (head args)
    let
        (a, b) = head $ filter (\(x, y) -> x + y == 2020) (pairs l :: [(Int, Int)])
        (c, (d, e)) = head $ filter (\(x, (y, z)) -> x + y + z == 2020) (triples l)

    printf "Pairs: found %d + %d = %d. Their product is %d\n" a b (a + b) (a * b)
    printf "Triples: found %d + %d + %d = %d. Their product is %d\n" c d e (c + d + e) (c * d * e)
