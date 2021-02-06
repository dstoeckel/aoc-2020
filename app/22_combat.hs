module Main where

import System.Environment
import qualified Deque.Strict as D
import qualified Data.Set as S
import GHC.Exts (IsList(..))
import Debug.Trace

type Log = S.Set (D.Deque Int)
data Winner = Player1 | Player2 deriving (Show)

-- Some stuff to supplement the deque implementation

instance (Ord a) => Ord (D.Deque a) where
    (<=) xs ys = case (D.uncons xs, D.uncons ys) of
        (Nothing, _) -> True
        (_, Nothing) -> False
        (Just (a, as), Just (b, bs)) -> case compare a b of
            EQ -> as <= bs
            LT -> True
            GT -> False

snoc2 :: D.Deque a -> a -> a -> D.Deque a
snoc2 xs a b = D.snoc b (D.snoc a xs)

takeD :: Int -> D.Deque a -> D.Deque a
takeD 0 _ = fromList []
takeD i xs = case D.uncons xs of
    Just (a, as) -> D.cons a (takeD (i - 1) as)
    Nothing -> xs

-- Implementation Part 1
run :: D.Deque Int -> D.Deque Int -> (Winner, D.Deque Int)
run xs ys =
    case (D.uncons xs, D.uncons ys) of
        (Nothing, Just (b, bs)) -> (Player2, ys)
        (Just (a, as), Nothing) -> (Player1, xs)
        (Just (a, as), Just (b, bs)) -> case compare a b of
            EQ -> undefined
            GT -> run (snoc2 as a b) bs
            LT -> run as (snoc2 bs b a)

-- Implementation Part 2
runRecursive :: D.Deque Int -> D.Deque Int -> Log -> Log -> (Winner, D.Deque Int)
runRecursive xs ys prevX prevY =
    let
        prevX' = S.insert xs prevX
        prevY' = S.insert ys prevY
    in
        if S.member xs prevX && S.member ys prevY then
            (Player1, xs)
        else case (D.uncons xs, D.uncons ys) of
            (Nothing, Just (b, bs)) -> (Player2, ys)
            (Just (a, as), Nothing) -> (Player1, xs)
            (Just (a, as), Just (b, bs)) ->
                let
                    winner = if a <= length as && b <= length bs
                        then fst $ runRecursive (takeD a as) (takeD b bs) S.empty S.empty
                        else if a > b then Player1 else Player2
                in
                    case winner of
                        Player1 -> runRecursive (snoc2 as a b) bs prevX' prevY'
                        Player2 -> runRecursive as (snoc2 bs b a) prevX' prevY'

main :: IO ()
main = do
    args <- getArgs
    l <- lines <$> readFile (head args)
    let
        (a, b) = span (/="") l
        player1 = fromList . map read . tail $ a
        player2 = fromList . map read . drop 2 $ b
        (_, result1) = run player1 player2
        (_, result2) = runRecursive player1 player2 S.empty S.empty

    -- Part 1
    print $ sum $ zipWith (*) [1..] (reverse . toList $ result1)
    -- Part 2
    print $ sum $ zipWith (*) [1..] (reverse . toList $ result2)
