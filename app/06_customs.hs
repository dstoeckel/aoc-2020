module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        forms = map (map Set.fromList . lines) . Split.splitOn "\n\n" $ raw
        anyone = map (foldl1 Set.union) forms
        common = map (foldl1 Set.intersection) forms

    putStrLn $ "Sum of questions anyone answered: " ++ show (sum $ map length anyone)
    putStrLn $ "Sum of questions everyone answered: " ++ show (sum $ map length common)
