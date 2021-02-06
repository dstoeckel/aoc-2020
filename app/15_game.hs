module Main where

import System.Environment
import Text.Read
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

initialize :: [Int] -> (Map.Map Int Int, Int)
initialize words = (Map.fromList (zip (init words) [1..]), last words)

gameStep (i, game, next) =
    case Map.lookup next game of
        Nothing -> (i + 1, game', 0)
        Just n -> (i + 1, game', i - n)
    where game' = Map.insert next i game

runGame i game next = iterate gameStep (i, game, next)

main :: IO ()
main = do
    args <- getArgs
    words <- map read <$> splitOn "," <$> readFile (head args) :: IO [Int]

    let
        l = length words
        (game, next) = initialize words
        (l', _, next') = runGame l game next !! (30000000 - l)

    print $ (l', next')
