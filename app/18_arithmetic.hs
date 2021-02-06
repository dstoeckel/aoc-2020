module Main where

import System.Environment
import Text.Printf
import Data.List (span, transpose, find, delete, isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Token = Number Int | Plus | Times | LBracket | RBracket deriving (Show, Eq)
data Expression = Add Expression Expression | Constant Int | Multiply Expression Expression deriving (Show)

evaluate :: Expression -> Int
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Multiply a b) = evaluate a * evaluate b
evaluate (Constant a) = a

lexer :: String -> [Token]
lexer [] = []
lexer (' ':ss) = lexer ss
lexer ('\n':ss) = lexer ss
lexer ('(':ss) = LBracket : lexer ss
lexer (')':ss) = RBracket : lexer ss
lexer ('+':ss) = Plus : lexer ss
lexer ('*':ss) = Times : lexer ss
lexer ss =
    let
        [(n, ss')] = reads ss
    in
        Number n : lexer ss'

-- Grammar Pt 1:
--  Expression := [Expression ('+'|'*')] BracketExpression
--  BracketExpression := '(' Expression ')' | [1-9][0-9]*
--
-- Grammar Pt 2:
--  Mult := Add ['*' Mult]
--  Add := BracketExpression ['+' Add]
--  BracketExpression := '(' Mult ')' | [1-9][0-9]*

-- Part 1
makeBinary :: Token -> Expression -> Expression -> Expression
makeBinary Plus e1 e2 = Add e1 e2
makeBinary Times e1 e2 = Multiply e1 e2
makeBinary x _ _ = error $ "Unexpected token " ++ show x

parseOperatorLR :: [Token] -> ([(Token, Expression)], [Token])
parseOperatorLR (op : ts) | op == Plus || op == Times =
    let
        (expr, ts') = parseBracket ts parseOperator
        (exprs, ts'') = parseOperatorLR ts'
    in
        ((op, expr) : exprs, ts'')
parseOperatorLR ts = ([], ts)

parseOperator :: [Token] -> (Expression, [Token])
parseOperator ts =
    case parseBracket ts parseOperator of
        (expr, op : ts') | op == Plus || op == Times ->
            let
                (exprs, ts'') = parseOperatorLR (op:ts')
                combined = foldl (\s (t, e) -> makeBinary t s e) expr exprs
            in
                (combined, ts'')
        r -> r
-- End Part 1

-- Part 2
parseMultiply :: [Token] -> (Expression, [Token])
parseMultiply ts =
    case parseAdd ts of
        (expr, Times : ts') ->
            let
                (expr', ts'') = parseMultiply ts'
            in
                (Multiply expr expr', ts'')
        r -> r

parseAdd :: [Token] -> (Expression, [Token])
parseAdd ts =
    case parseBracket ts parseMultiply of
        (expr, Plus : ts') ->
            let
                (expr', ts'') = parseAdd ts'
            in
                (Add expr expr', ts'')
        r -> r

-- End Part2

parseBracket :: [Token] -> ([Token] -> (Expression, [Token])) -> (Expression, [Token])
parseBracket (Number a : ts) _ = (Constant a, ts)
parseBracket (LBracket : ts) next = case next ts of
    (expr, RBracket : ts') -> (expr, ts')
    (_, ts') -> error $ "Unbalanced Parenthesis: " ++ show ts'

parser ::  ([Token] -> (Expression, [Token])) -> [Token] -> Expression
parser p ts = case p ts of
    (expr, []) -> expr
    (_, xs) -> error $ "Unconsumed tokens left after parsing" ++ show xs

main :: IO ()
main = do
    args <- getArgs
    tokens <- map lexer <$> lines <$> readFile (head args)

    print $ sum . map (evaluate . parser parseOperator) $ tokens
    print $ sum . map (evaluate . parser parseMultiply) $ tokens
