{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MonkeyMath (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Text.Megaparsec (endBy, (<|>))
import Text.Megaparsec.Char (char, lowerChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Monkey = MonkeyYell Int | MonkeyAdd String String | MonkeyMul String String | MonkeySub String String | MonkeyDiv String String | MonkeyUnknown deriving (Generic, NFData)

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr | Unknown deriving (Generic, NFData)

parseInput :: Parser (M.Map String Monkey)
parseInput = M.fromList <$> line `endBy` newline
  where
    line = (,) <$> name <* char ':' <* space <*> monkey
    name = replicateM 4 lowerChar
    monkey = MonkeyYell <$> decimal <|> flip ($) <$> name <* space <*> operation <* space <*> name
    operation = MonkeyAdd <$ char '+' <|> MonkeyMul <$ char '*' <|> MonkeySub <$ char '-' <|> MonkeyDiv <$ char '/'

reduceOp :: (Expr -> Expr -> Expr) -> Expr -> Expr -> (Int -> Int -> Int) -> Expr
reduceOp constr a b op = case (reduce a, reduce b) of
  (Lit a', Lit b') -> Lit (op a' b')
  (a', b') -> constr a' b'

reduce :: Expr -> Expr
reduce (Add a b) = reduceOp Add a b (+)
reduce (Mul a b) = reduceOp Mul a b (*)
reduce (Sub a b) = reduceOp Sub a b (-)
reduce (Div a b) = reduceOp Div a b div
reduce expr = expr

solveEquation :: Expr -> Expr -> Int
solveEquation exprA exprB = case (reduce exprA, reduce exprB) of
  (Lit _, Lit _) -> undefined
  (Lit a, b) -> solve b a
  (a, Lit b) -> solve a b
  _ -> undefined

solve :: Expr -> Int -> Int
solve (Lit _) _ = undefined
solve (Add a b) solution = case (reduce a, reduce b) of
  (Lit _, Lit _) -> undefined
  (Lit a', b') -> solve b' (solution - a')
  (a', Lit b') -> solve a' (solution - b')
  _ -> undefined
solve (Mul a b) solution = case (reduce a, reduce b) of
  (Lit _, Lit _) -> undefined
  (Lit a', b') -> solve b' (solution `div` a')
  (a', Lit b') -> solve a' (solution `div` b')
  _ -> undefined
solve (Sub a b) solution = case (reduce a, reduce b) of
  (Lit _, Lit _) -> undefined
  (Lit a', b') -> solve b' (a' - solution)
  (a', Lit b') -> solve a' (solution + b')
  _ -> undefined
solve (Div a b) solution = case (reduce a, reduce b) of
  (Lit _, Lit _) -> undefined
  (Lit a', b') -> solve b' (a' `div` solution)
  (a', Lit b') -> solve a' (solution * b')
  _ -> undefined
solve Unknown solution = solution

buildExpr :: String -> M.Map String Monkey -> Expr
buildExpr name monkeys = case monkeys M.! name of
  MonkeyYell n -> Lit n
  MonkeyAdd nameA nameB -> Add (buildExpr nameA monkeys) (buildExpr nameB monkeys)
  MonkeyMul nameA nameB -> Mul (buildExpr nameA monkeys) (buildExpr nameB monkeys)
  MonkeySub nameA nameB -> Sub (buildExpr nameA monkeys) (buildExpr nameB monkeys)
  MonkeyDiv nameA nameB -> Div (buildExpr nameA monkeys) (buildExpr nameB monkeys)
  MonkeyUnknown -> Unknown

operands :: Expr -> (Expr, Expr)
operands (Add a b) = (a, b)
operands (Mul a b) = (a, b)
operands (Sub a b) = (a, b)
operands (Div a b) = (a, b)
operands _ = undefined

part1 :: M.Map String Monkey -> String
part1 = show . solveEquation Unknown . buildExpr "root"

part2 :: M.Map String Monkey -> String
part2 = show . uncurry solveEquation . operands . buildExpr "root" . M.insert "humn" MonkeyUnknown
