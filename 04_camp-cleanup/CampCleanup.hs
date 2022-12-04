module CampCleanup (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Section = Int

type SectionRange = (Section, Section)

parseInput :: Parser [(SectionRange, SectionRange)]
parseInput = ((,) <$> sectionRange <* char ',' <*> sectionRange) `endBy` newline
  where
    sectionRange = (,) <$> decimal <* char '-' <*> decimal

fullyContains :: SectionRange -> SectionRange -> Bool
fullyContains (x1, x2) (y1, y2) = x1 <= y1 && x2 >= y2 || y1 <= x1 && y2 >= x2

part1 :: [(SectionRange, SectionRange)] -> String
part1 = show . length . filter (uncurry fullyContains)

part2 :: [(SectionRange, SectionRange)] -> String
part2 = undefined
