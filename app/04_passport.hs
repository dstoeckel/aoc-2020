module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import Data.Char

replace :: Char -> Char -> String -> String
replace from to xs = map (\x -> if x == from then to else x) xs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

parseRecord :: String -> (String, String)
parseRecord = toPair . Split.splitOn ":"

isValidHeight :: String -> Bool
isValidHeight s =
    let
        (x, y) = span isDigit s
        (x', y') = (readMaybe x, y)
    in
        case (x', y') of
            (Just x, "cm") -> x >= 150 && x <= 193
            (Just x, "in") -> x >= 59 && x <= 76
            _ -> False

isValidHairColor :: String -> Bool
isValidHairColor ('#':xs) = length xs == 6 && all isHexDigit xs
isValidHairColor _ = False

isValidEyeColor :: String -> Bool
isValidEyeColor x = elem x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValid :: (String, String) -> Bool
isValid ("byr", x) =
    case readMaybe x of
        Just x' -> x' >= 1920 && x' <= 2002
        _ -> False
isValid ("iyr", x) =
    case readMaybe x of
        Just x' -> x' >= 2010 && x' <= 2020
        _ -> False
isValid ("eyr", x) =
    case readMaybe x of
        Just x' -> x' >= 2020 && x' <= 2030
        _ -> False
isValid ("hgt", x) = isValidHeight x
isValid ("hcl", x) = isValidHairColor x
isValid ("ecl", x) = isValidEyeColor x
isValid ("pid", x) = length x == 9 && all isDigit x
isValid ("cid", _) = True

countRequiredFields f = length . filter (\(x, _) -> Set.member x f)

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        requiredFields = Set.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
        l = map words . map (replace '\n' ' ') . Split.splitOn "\n\n" $ raw

        records = map (map parseRecord) l
        counts = map (countRequiredFields requiredFields) records

    print (length . filter (==7) $ counts)
    print (length . filter (\xs -> countRequiredFields requiredFields xs == 7 && all isValid xs) $ records)
