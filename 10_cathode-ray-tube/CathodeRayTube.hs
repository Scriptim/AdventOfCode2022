{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CathodeRayTube (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import Data.List.Extra (chunksOf)
import Data.Text (pack)
import GHC.Generics (Generic)
import Text.Megaparsec (endBy, (<|>))
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction = Noop | AddX Int deriving (Generic, NFData)

parseInput :: Parser [Instruction]
parseInput = instruction `endBy` newline
  where
    instruction = (Noop <$ string (pack "noop")) <|> (AddX <$> (string (pack "addx ") *> signed (return ()) decimal))

execute :: [Instruction] -> [Int]
execute = (1 :) . go 1
  where
    go x [] = [x]
    go x (Noop : instrs) = x : go x instrs
    go x (AddX n : instrs) = x : (x + n) : go (x + n) instrs

signalStrengthsSum :: [Int] -> Int
signalStrengthsSum = sum . (flip map [19, 59 .. 219] . (!!)) . zipWith (*) [1 ..]

draw :: [Int] -> [String]
draw = chunksOf 40 . zipWith drawPixel [0 .. 239]
  where
    drawPixel pixel spriteCenter = if abs (pixel `mod` 40 - spriteCenter `rem` 40) <= 1 then '#' else '.'

part1 :: [Instruction] -> String
part1 = show . signalStrengthsSum . execute

part2 :: [Instruction] -> String
part2 = unlines . ([] :) . draw . execute
