module Main where

import System.Environment
import Text.Read
import Data.List (find)
import Data.List.Split (splitOn)

countChar :: Char -> String -> Int
countChar c d = length $ filter (==c) d

nextDate :: Int -> Int -> Int
nextDate e s =
    (n + 1) * s
    where n = div e s

gcdx :: Int -> Int -> (Int, Int, Int)
gcdx a b =
    if b == 0 then (a, 1, 0)
    else
        let
            (d, r) = divMod a b
            -- g =       u*b + v*r
            --   =       u*b + v*(a - d*b)
            --   = (u-v*d)*b + v*a
            (g, u, v) = gcdx b r
        in
            (g, v, u - v*d)

-- Uses the Chinese Remainder Theorem to compute the
chineseStep :: Int -> (Int, Int) -> Int
chineseStep m (x, y) =
    let
        mi = div m y
        (_,_,s) = gcdx y mi
    in
        - x * s * mi

main :: IO ()
main = do
    args <- getArgs
    [e, s] <- lines <$> readFile (head args)
    let
        earliest = read e
        schedule = map (\(x,y) -> (x,read y)) . filter ((/="x") . snd) . zip [0..]. splitOn "," $ s

        -- Part I
        nextDates = map ((nextDate earliest) . snd) schedule
        (time, bus) = minimum (zip nextDates (map snd schedule))

    -- Part I
    print $ (time - earliest) * bus

    -- Part II
    -- Effectively we need to solve a set of modular equations. For this we use
    -- the Chinese Remainder Theorem.
    -- The core idea is that we have a set of recurrences
    --     t ≡ - s_i mod b_i
    -- where t is the timestamp we search, s_i is the delta the busses should
    -- depart at and b_i is the bus id. We can exploit the fact that the busses
    -- have prime ids (Thanks!) and solve the equation wrt. B = b_1 * b_2 * ... * b_n
    -- For each equation B_i = B / b_i and b_i do not share a devisor and we can compute
    --     u_i * B_i + v_i * b_i = 1
    -- Note that u_i * B_i ≡ 1 mod b_i and u_i * M_i ≡ 0 mod b_j, i /= j
    -- Thus if we sum up all u_i * B_i we solve all equations ≡ 1
    -- We simply multiply each term with the s_i to get the timestamp we are
    -- looking for (mod B)

    let
        b = foldl1 lcm $ map snd schedule
        t = sum $ map (chineseStep b) schedule

    print $ mod t b

