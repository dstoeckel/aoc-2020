module Main where

import System.Environment
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Set as Set

import Data.Char
import Data.Array

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int

parseInstruction :: [String] -> Instruction
parseInstruction [x,y] =
    case x of
        "nop" -> Nop y'
        "acc" -> Acc y'
        "jmp" -> Jmp y'
    where y' = read y

execute :: Array Int Instruction -> Either String Int
execute instr =
    let
        bnds = bounds instr
        alreadyExecuted = array bnds (zip (range bnds) [False| i <- range bnds])
        execute' i acc alreadyExecuted =
            if i >= length instr then
                Right acc
            else if alreadyExecuted ! i then
                Left $ "Infinite Loop detected! Accumulator was " ++ show acc
            else
                case instr ! i of
                    Nop _ -> execute' (i + 1) acc alreadyExecuted'
                    Acc x -> execute' (i + 1) (acc + x) alreadyExecuted'
                    Jmp x -> execute' (i + x) acc alreadyExecuted'
            where alreadyExecuted' = alreadyExecuted // [(i,True)]
    in
        execute' 0 0 alreadyExecuted

tryFlipInstructions :: Array Int Instruction -> Maybe Int
tryFlipInstructions instr =
    let
        tryFlipInstructions' :: Int -> Maybe Int
        tryFlipInstructions' i
            | i >= length instr = Nothing
            | otherwise =
                let
                    result = case instr ! i of
                        Nop x -> Just $ execute (instr // [(i, Jmp x)])
                        Jmp x -> Just $ execute (instr // [(i, Nop x)])
                        _ -> Nothing
                in
                    case result of
                        Just (Right x) -> Just x
                        Just (Left _) -> tryFlipInstructions' (i + 1)
                        Nothing -> tryFlipInstructions' (i + 1)
    in
        tryFlipInstructions' 0

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ head args
    let
        instructions = map (parseInstruction . words . filter (/= '+')) . lines $ raw
        instructions' = array (0, length instructions - 1) (zip [0..] instructions)

    print $ execute instructions'
    print $ tryFlipInstructions instructions'
