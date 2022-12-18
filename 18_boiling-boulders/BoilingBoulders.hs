module BoilingBoulders (parseInput, part1, part2) where

import AdventOfCode (Parser)
import qualified Data.Set as S
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Cube = (Int, Int, Int)

parseInput :: Parser (S.Set Cube)
parseInput = S.fromList <$> cube `endBy` newline
  where
    cube = (,,) <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

neighbors :: Cube -> [Cube]
neighbors (x, y, z) = [(pred x, y, z), (succ x, y, z), (x, pred y, z), (x, succ y, z), (x, y, pred z), (x, y, succ z)]

exposedSurface :: S.Set Cube -> Int
exposedSurface cubes = length . concatMap (filter (`S.notMember` cubes) . neighbors) $ S.toList cubes

part1 :: S.Set Cube -> String
part1 = show . exposedSurface

part2 :: S.Set Cube -> String
part2 = undefined
