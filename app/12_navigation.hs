module Main where

import System.Environment
import Text.Printf
import Data.Array
import Data.List (find)
import Data.List.Split (chunksOf)

type Position = (Int, Int)
type Waypoint = (Int, Int)
data Heading = HNorth | HSouth | HEast | HWest deriving(Show)

data Instruction = North Int | South Int | East Int | West Int | L Int | R Int | Forward Int

data State = State Heading Position deriving(Show)
data State' = State' Position Position deriving(Show)

parseInstruction :: String -> Instruction
parseInstruction (x:xs) =
    let
        instr = case x of
            'N' -> North
            'S' -> South
            'E' -> East
            'W' -> West
            'L' -> L
            'R' -> R
            'F' -> Forward
    in
        instr (read xs)

move :: Heading -> Int -> Position -> Position
move HNorth n (x, y) = (x + n, y    )
move HSouth n (x, y) = (x - n, y    )
move HEast  n (x, y) = (x    , y + n)
move HWest  n (x, y) = (x    , y - n)

rotate :: Instruction -> Heading -> Heading
rotate (L 0) h = h
rotate (R 0) h = h
rotate (L x) HNorth = rotate (L (x - 90)) HWest
rotate (R x) HNorth = rotate (R (x - 90)) HEast
rotate (L x) HSouth = rotate (L (x - 90)) HEast
rotate (R x) HSouth = rotate (R (x - 90)) HWest
rotate (L x) HEast  = rotate (L (x - 90)) HNorth
rotate (R x) HEast  = rotate (R (x - 90)) HSouth
rotate (L x) HWest  = rotate (L (x - 90)) HSouth
rotate (R x) HWest  = rotate (R (x - 90)) HNorth

rotate' :: Instruction -> Waypoint -> Waypoint
rotate' (L 0) w = w
rotate' (R 0) w = w
rotate' (L n) (wx, wy) = rotate' (L (n - 90)) ( wy, -wx)
rotate' (R n) (wx, wy) = rotate' (R (n - 90)) (-wy,  wx)

runInstruction :: State -> Instruction -> State
runInstruction (State h p) i = case i of
    North n -> State h (move HNorth n p)
    South n -> State h (move HSouth n p)
    East  n -> State h (move HEast  n p)
    West  n -> State h (move HWest  n p)
    Forward n -> State h (move h n p)
    turn -> State (rotate turn h) p

addN :: Int -> Position -> Waypoint -> Position
addN n (px, py) (wx, wy) = (px + n*wx, py + n*wy)

runInstructionActual :: State' -> Instruction -> State'
runInstructionActual (State' p w) i = case i of
    North n -> State' p (move HNorth n w)
    South n -> State' p (move HSouth n w)
    East  n -> State' p (move HEast  n w)
    West  n -> State' p (move HWest  n w)
    Forward n -> State' (addN n p w) w
    turn -> State' p (rotate' turn w)

--abs :: Int -> Int
--abs x = if x > 0 then x else -x

manhattenDistance (x1, x2) (y1, y2) = abs (x1 - y1) + abs (x2 - y2)

main :: IO ()
main = do
    args <- getArgs
    instructions <- map parseInstruction <$> lines <$> readFile (head args)
    let
        origin = State HEast (0, 0)
        origin' = State' (0, 0) (1, 10)
        State h p = foldl runInstruction origin instructions
        State' p' w = foldl runInstructionActual origin' instructions

    print $ State h p
    print $ manhattenDistance (0, 0) p

    print origin
    print $ State' p' w
    print $ manhattenDistance (0, 0) p'
