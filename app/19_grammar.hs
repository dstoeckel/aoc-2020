module Main where

import System.Environment
import Data.List (find, mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Char (isAlpha, isDigit)

data Rule = Disjunction [Rule] | Sequence [Int] | Terminal Char | Rule0 Int (Int, Int) deriving (Show)
data Token = Bar | Number Int | Character Char deriving (Show, Eq)

(!) = (M.!)

iterateWhile :: Int -> (a -> (Bool, a)) -> a -> (Bool, a)
iterateWhile 0 _ x = (True, x)
iterateWhile i f x =
    case f x of
        (True, x') -> iterateWhile (i - 1) f x'
        r -> r

matchSame :: M.Map Int Rule -> Rule -> Rule -> Rule -> String -> (Int, Int) -> Bool
matchSame mr ri ra rb xs (i, j) =
    let
        (t, xs') = iterateWhile i (matches mr ri) xs
        (t', xs'') = iterateWhile j (matches mr ra) xs'
        (t'', xs''') = iterateWhile j (matches mr rb) xs''
    in
        t && t' && t'' && xs''' == ""

matches :: M.Map Int Rule -> Rule -> String -> (Bool, String)
matches mr (Terminal t) [] = (False, [])
matches mr (Terminal t) (x:xs) = (t == x, xs)
matches mr (Sequence []) xs = (True, xs)
matches mr (Sequence (i:is)) xs =
    let
        (t, xs') = matches mr (mr ! i) xs
    in
        if t then matches mr (Sequence is) xs' else (False, xs')

matches mr (Disjunction []) xs = (False, xs)
matches mr (Disjunction (r:rs)) xs =
    let
        (t, xs') = matches mr r xs
    in
        if t then (True, xs') else matches mr (Disjunction rs) xs

matches mr (Rule0 i (a, b)) xs =
    let
        Just ri = M.lookup i mr
        Just ra = M.lookup a mr
        Just rb = M.lookup b mr
        l = length xs
    in
        case find (matchSame mr ri ra rb xs) [(i,j) | i <- [1..l - 1], j <- [1..l - 1], i + j <= l] of
            Just _ -> (True, "")
            Nothing -> (False, xs)


doesMatch :: M.Map Int Rule -> Rule -> String -> Bool
doesMatch mr r s = let (t, xs) = matches mr r s in t && xs == ""

indexLine :: String -> (Int, String)
indexLine s =
    let
        [i, s'] = splitOn ": " s
    in
        (read i, s')

index :: [String] -> M.Map Int String
index = M.fromList . map indexLine

lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('|':xs) = Bar : lexer xs
lexer ['"', c, '"'] | isAlpha c = [Character c]
lexer (x:xs) | isDigit x = let [(n, xs')] = reads (x:xs) in Number n : lexer xs'
lexer s = error $ "Unexpected input: '" ++ s ++ "'"

parseSequence :: [Token] -> Rule
parseSequence [Character c] = Terminal c
parseSequence ts = Sequence . map (\(Number n) -> n) $ ts

parse :: [Token] -> Rule
parse = Disjunction . map parseSequence . splitOn [Bar]

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)

    let
        rules = M.map (parse . lexer) . index . takeWhile (/="") $ l
        sequences = tail $ dropWhile (/="") l
        Just rule0 = M.lookup 0 rules
        Just rule42 = M.lookup 42 rules

        -- 8: 42 | 42 8
        -- 11: 42 31 | 42 11 31
        rule0' = Rule0 42 (42, 31)

    print $ length . filter (doesMatch rules rule0) $ sequences
    print $ length . filter (doesMatch rules rule0') $ sequences
