{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MonkeyInTheMiddle (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.Text (pack)
import GHC.Generics (Generic)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Monkey = Monkey
  { itemQueue :: [Int],
    operation :: Int -> Int,
    testDivisor :: Int,
    ifTrueMonkey :: Int,
    ifFalseMonkey :: Int
  }
  deriving (Generic, NFData)

parseInput :: Parser [Monkey]
parseInput = monkeyP `sepBy` newline
  where
    monkeyP = string (pack "Monkey ") *> (decimal :: Parser Int) *> char ':' *> newline *> monkeyBodyP <* newline
    monkeyBodyP = Monkey <$> startingItemsP <*> operationP <*> testDivisorP <*> testActionP <*> testActionP
    startingItemsP = space *> string (pack "Starting items: ") *> (decimal `sepBy` (char ',' *> space))
    operationP = buildOperation <$> (space *> string (pack "Operation: new = ") *> operandP <* space) <*> (operatorP <* space) <*> operandP
    buildOperation op1 op op2 old = op1 old `op` op2 old
    operandP = (const <$> decimal) <|> (id <$ string (pack "old"))
    operatorP = ((+) <$ char '+') <|> ((*) <$ char '*')
    testDivisorP = space *> string (pack "Test: divisible by ") *> decimal
    testActionP = space *> testActionPrefixP *> decimal
    testActionPrefixP = string (pack "If true: throw to monkey ") <|> string (pack "If false: throw to monkey ")

inspectAndThrow :: Bool -> Int -> M.IntMap (Monkey, Int) -> Int -> M.IntMap (Monkey, Int)
inspectAndThrow bore ring monkeys monkeyId =
  let (monkey, inspections) = monkeys M.! monkeyId
   in case itemQueue monkey of
        [] -> monkeys
        (item : items) -> throw . inspect $ monkeys
          where
            inspect = M.insert monkeyId (monkey {itemQueue = items}, inspections + 1)
            throw = M.insert throwTo (throwToMonkey {itemQueue = itemQueue throwToMonkey ++ [worryLevel]}, throwToMonkeyInspections)
            throwTo = if worryLevel `mod` testDivisor monkey == 0 then ifTrueMonkey monkey else ifFalseMonkey monkey
            (throwToMonkey, throwToMonkeyInspections) = monkeys M.! throwTo
            worryLevel = operation monkey item `div` (if bore then 3 else 1) `mod` ring

monkeyRound :: Bool -> Int -> M.IntMap (Monkey, Int) -> M.IntMap (Monkey, Int)
monkeyRound bore ring = go =<< enumFromTo 0 . pred . M.size
  where
    go [] monkeys = monkeys
    go monkeyIds@(monkeyId : rest) monkeys
      | null . itemQueue . fst $ monkeys M.! monkeyId = go rest monkeys
      | otherwise = go monkeyIds (inspectAndThrow bore ring monkeys monkeyId)

countInspections :: Bool -> Int -> [Monkey] -> [Int]
countInspections bore rounds monkeys = map (snd . snd) . M.toList $ iterate (monkeyRound bore ring) monkeyMap !! rounds
  where
    monkeyMap = M.fromList . zip [0 ..] $ zip monkeys (repeat 0)
    ring = foldl1 lcm $ map testDivisor monkeys

part1 :: [Monkey] -> String
part1 = show . product . take 2 . sortBy (flip compare) . countInspections True 20

part2 :: [Monkey] -> String
part2 = show . product . take 2 . sortBy (flip compare) . countInspections False 10000
