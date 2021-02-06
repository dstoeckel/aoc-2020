module Main where

import System.Environment
import Text.Read
import qualified Data.Map as Map
import Data.Bits (shift, shiftR, (.&.), (.|.), complement)
import Data.List.Split (splitOn)


data Instruction
    = Mem { dest :: Int, value :: Int }
    | Mask { ones :: Int, zeros :: Int, floating :: [Int] }
    deriving (Show)

type WorkArea = Map.Map Int Int

parseMask :: String -> Instruction
parseMask s = buildMask $ zip [0..] (reverse s)

buildMask :: [(Int, Char)] -> Instruction
buildMask indexed =
    let
        a = sum $ map ((shift 1) . fst) . filter ((=='1') . snd) $ indexed
        b = complement . sum $ map ((shift 1) . fst) . filter ((=='0') . snd) $ indexed
        c = map fst . filter ((=='X') . snd) $ indexed
    in
        Mask a b c

parseMemory :: String -> String -> Instruction
parseMemory s t =
    let
        (dest, _) = head . reads $ drop 4 s
        value = read t
    in
        Mem dest value

parseInstruction :: [String] -> Instruction
parseInstruction ["mask", t] = parseMask t
parseInstruction [s, t] = parseMemory s t

applyMaskV1 value mask = (value .|. ones mask) .&. zeros mask

isSet :: Int -> Int -> Char
isSet m i = if m .&. shift 1 i == 0 then '0' else '1'

expandFloating :: Instruction -> [Instruction]
expandFloating mask =
    let
        imask = zip (floating mask) [0..]
        l = length imask
    in
        map (\i -> buildMask $ map (\(m, j) -> (m, isSet i j)) imask) [0..(shift 1 l - 1)]

runInstructionV1 :: (WorkArea, Instruction) -> Instruction -> (WorkArea, Instruction)
runInstructionV1 (memory, mask) (Mem d v) = (Map.insert d (applyMaskV1 v mask) memory, mask)
runInstructionV1 (memory, _) mask = (memory, mask)

runInstructionV2 :: (WorkArea, Instruction) -> Instruction -> (WorkArea, Instruction)
runInstructionV2 (memory, mask) (Mem d v) =
    let
        d' = d .|. ones mask
        ds = map (\m -> applyMaskV1 d' m) . expandFloating $ mask
    in
        (foldl (\s x -> Map.insert x v s) memory ds, mask)
runInstructionV2 (memory, _) mask = (memory, mask)

runCode :: ((WorkArea, Instruction) -> Instruction -> (WorkArea, Instruction)) -> [Instruction] -> Int
runCode f instr =
    let
        (memory, _) = foldl f (Map.empty, Mask (complement 0) 0 []) instr
    in
        Map.foldr (+) 0 memory

main :: IO ()
main = do
    args <- getArgs
    instr <- map (parseInstruction . splitOn " = ") <$> lines <$> readFile (head args)

    print $ runCode runInstructionV1 instr
    print $ runCode runInstructionV2 instr
