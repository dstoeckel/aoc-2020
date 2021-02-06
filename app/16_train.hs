module Main where

import System.Environment
import Text.Printf
import Data.List (span, transpose, find, delete, isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Ticket = [Int]
data Ranges = Ranges { name :: String, fields :: [(Int, Int)] } deriving (Show)

combination x ys = foldr ((:) . (,) x) [] ys

pairs [] = []
pairs (x:xs) = combination x xs ++ pairs xs

triples [] = []
triples (x:xs) = combination x (pairs xs) ++ triples xs

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseRange :: String -> (Int, Int)
parseRange s =
    let
        [a, b] = map read . splitOn "-" $ s
    in
        (a, b)

parseRanges :: String -> Ranges
parseRanges s =
    let
        [name, def] = splitOn ": " s
    in
        Ranges name (map parseRange . splitOn " or " $ def)

inRange :: (Ord a) => a -> (a, a) -> Bool
inRange v (a, b) = v >= a && v <= b

isValid :: Int -> Ranges -> Bool
isValid v r = any (inRange v) (fields r)

invalidValues :: [Ranges] -> Ticket -> [Int]
invalidValues rs t = filter (\v -> all (not . isValid v) rs) t

hasOnlyValidValues :: [Ranges] -> Ticket -> Bool
hasOnlyValidValues rs = null . invalidValues rs

validFor :: Ranges -> [Int] -> Bool
validFor r = all (\v -> any (inRange v) (fields r))

validPositions :: [[Int]] -> Ranges -> (String, [Int])
validPositions ps r = (name r, map fst . filter (\(i, p) -> validFor r p) $ (zip [0..] ps))

strip :: String -> Int -> [(String, [Int])] -> [(String, [Int])]
strip name x xs =
    let
        xs' = delete (name, [x]) xs
    in
        map (\(n, v) -> (n, delete x v)) xs'

reduce :: [(String, [Int])] -> Map.Map String Int -> Map.Map String Int
reduce [] assignment = assignment
reduce xs assignment =
    case find ((==1) . length . snd) xs of
        Nothing -> undefined
        Just (name, [x]) -> reduce (strip name x xs) (Map.insert name x assignment)

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)
    let
        (fieldsRaw, l') = span (/="") l
        fields = map parseRanges fieldsRaw
        ownTicket = parseTicket . head . drop 2 $ l'
        otherTickets = map parseTicket . drop 5 $ l'
        allValidTickets = filter (hasOnlyValidValues fields) (ownTicket : otherTickets)
        positionValues = map (validPositions . transpose $ allValidTickets) fields
        assignments = reduce positionValues Map.empty

    print $ sum $ otherTickets >>= invalidValues fields
    print assignments
    print $ Map.filterWithKey (\k v -> isPrefixOf "departure" k) $ assignments
    print $ Map.foldl (\s i -> (ownTicket !! i) * s) 1 . Map.filterWithKey (\k v -> isPrefixOf "departure" k) $ assignments

