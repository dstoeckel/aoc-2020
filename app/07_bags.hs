module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Set as Set

import qualified Data.List.Split as Split
import Data.Char

data Graph a b = Node
    { source :: a
    , target :: a
    , value :: b
    } deriving (Show)

parseContained :: [String] -> [(Int, String)]
parseContained [] = []
parseContained ["no", "other", "bags."]= []
parseContained (x:y:z:_:xs)= (read x, y ++ " " ++ z) : parseContained xs

parseRule :: [String] -> (String, [(Int, String)])
parseRule (x:y:"bags":"contain":xs) = (x ++ " " ++ y, parseContained xs)

possibleContainer :: String -> [Graph String Int] -> Set.Set String
possibleContainer x xs =
    let
        direct = filter (\n -> source n == x) xs
        indirect = foldl Set.union Set.empty $ map (\n -> possibleContainer (target n) xs) direct
    in
        Set.union (Set.fromList $ map target direct) indirect

mustContain :: String -> [Graph String Int] -> Int
mustContain x xs =
    let
        direct = filter ((==x) . target) xs
        indirect = map (\y -> (value y) * mustContain (source y) xs) direct
    in
        sum (map value direct) + sum indirect

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        bags = map (parseRule . words) . lines $ raw
        containedIn = bags >>= (\(x, ys) -> map (\(i, y) -> Node y x i) ys)

    print $ length $ possibleContainer "shiny gold" containedIn
    print $ mustContain "shiny gold" containedIn
