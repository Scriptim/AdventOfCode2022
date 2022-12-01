module CalorieCounting (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (endBy1, sepBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parser [[Int]]
parseInput = (decimal `endBy1` newline) `sepBy` newline

part1 :: [[Int]] -> String
part1 = show . maximum . map sum

part2 :: [[Int]] -> String
part2 = undefined
