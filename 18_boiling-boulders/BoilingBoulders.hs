module BoilingBoulders (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Foldable (Foldable (foldl'))
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

bounds :: S.Set Cube -> ((Int, Int), (Int, Int), (Int, Int))
bounds = go ((maxBound, minBound), (maxBound, minBound), (maxBound, minBound)) . S.toList
  where
    go accu [] = accu
    go ((xl, xu), (yl, yu), (zl, zu)) ((x, y, z) : cubes) = go ((min xl x, max xu x), (min yl y, max yu y), (min zl z, max zu z)) cubes

flood :: S.Set Cube -> S.Set Cube -> Cube -> S.Set Cube
flood cubes visited current = foldl' (flood cubes) (S.insert current visited) next
  where
    next = filter (\pos -> local pos && pos `S.notMember` visited && pos `S.notMember` cubes) $ neighbors current
    local (x, y, z) = x >= pred xl && x <= succ xu && y >= pred yl && y <= succ yu && z >= pred zl && z <= succ zu
    ((xl, xu), (yl, yu), (zl, zu)) = bounds cubes

exteriorSurface :: S.Set Cube -> Int
exteriorSurface cubes = length . concatMap (filter (`S.member` cubes) . neighbors) . S.toList $ reachable
  where
    reachable = flood cubes S.empty (pred xl, pred yl, pred zl)
    ((xl, _), (yl, _), (zl, _)) = bounds cubes

part1 :: S.Set Cube -> String
part1 = show . exposedSurface

part2 :: S.Set Cube -> String
part2 = show . exteriorSurface
