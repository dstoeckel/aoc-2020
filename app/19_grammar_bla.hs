module Main where

import System.Environment
import Data.List (find, mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Char (isAlpha, isDigit)

data Rule = Disjunction [Rule] | Sequence [Rule] | Terminal Char deriving (Show)
data Token = Bar | Number Int | Character Char deriving (Show, Eq)

matches' :: (Bool, String) -> Rule -> (Bool, String)
matches' (t, xs) r =
    let
        (t', xs') = matches r xs
    in
        (t && t', xs')

matches :: Rule -> String -> (Bool, String)
matches (Terminal t) [] = (False, [])
matches (Terminal t) (x:xs) = (t == x, xs)
matches (Sequence []) xs = (True, xs)
matches (Sequence (r:rs)) xs =
    let
        (t, xs') = matches r xs
    in
        if t then matches (Sequence rs) xs' else (False, xs')

matches (Disjunction rs) xs =
    let
        m = map (flip matches xs) rs
    in
        case find fst m of
            Just (t, xs') -> (t, xs')
            Nothing -> (False, xs)

doesMatch :: Rule -> String -> Bool
doesMatch r s = let (t, xs) = matches r s in t && xs == ""

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
lexer ('"':c:'"':xs) | isAlpha c = Character c : lexer xs
lexer (x:xs) | isDigit x = let [(n, xs')] = reads (x:xs) in Number n : lexer xs'
lexer s = error $ "Unexpected input: '" ++ s ++ "'"

parseItem :: M.Map Int String -> M.Map Int Rule -> Token -> (M.Map Int Rule, Rule)
parseItem ms mr (Number n) = parse ms mr n
parseItem ms mr (Character c) = (mr, Terminal c)
parseItem _ _ t = error $ "Unexpected token " ++ show t

parseSequence :: M.Map Int String -> M.Map Int Rule -> [Token] -> (M.Map Int Rule, Rule)
parseSequence ms mr ts =
    let
        (mr', rules) = mapAccumL (parseItem ms) mr ts
    in
        (mr', Sequence rules)

parse :: M.Map Int String -> M.Map Int Rule -> Int -> (M.Map Int Rule, Rule)
parse ms mr i =
    case M.lookup i mr of
        Just r -> (mr, r)
        Nothing ->
            let
                Just s = M.lookup i ms
                (mr', rules) = mapAccumL (parseSequence ms) mr . splitOn [Bar] . lexer $ s
                r = Disjunction rules
            in
                (M.insert i r mr', r)

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)

    let
        rules = index $ takeWhile (/="") l
        sequences = tail $ dropWhile (/="") l
        rule0 = snd $ parse rules M.empty 0

    print $ length . filter (doesMatch rule0) $ sequences
