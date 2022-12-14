module RegolithReservoir (parseInput, part1, part2) where

import AdventOfCode (Parser)
import qualified Data.Set as S
import Data.Text (pack)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Coordinate = (Int, Int)

parseInput :: Parser (S.Set Coordinate)
parseInput = S.fromList . concatMap structureCoordinates <$> structureEndpoints `sepBy` newline
  where
    line (x1, y1) (x2, y2) = [(x, y) | x <- [(min x1 x2) .. (max x1 x2)], y <- [(min y1 y2) .. (max y1 y2)]]
    structureCoordinates coords = concatMap (uncurry line) $ zip coords (tail coords)
    structureEndpoints = coordinate `sepBy` string (pack " -> ") :: Parser [Coordinate]
    coordinate = (,) <$> decimal <* char ',' <*> decimal :: Parser Coordinate

landingPosition :: S.Set Coordinate -> Int -> Coordinate -> Maybe Coordinate
landingPosition blocks abyss (x, y)
  | y > abyss = Nothing
  | S.notMember (x, succ y) blocks = landingPosition blocks abyss (x, succ y)
  | S.notMember (pred x, succ y) blocks = landingPosition blocks abyss (pred x, succ y)
  | S.notMember (succ x, succ y) blocks = landingPosition blocks abyss (succ x, succ y)
  | otherwise = Just (x, y)

produceSand :: S.Set Coordinate -> S.Set Coordinate
produceSand blocks = go blocks
  where
    abyss = maximum . map snd . S.toList $ blocks
    go blocks' = case landingPosition blocks' abyss (500, 0) of
      Nothing -> blocks'
      Just pos -> go $ S.insert pos blocks'

part1 :: S.Set Coordinate -> String
part1 rocks = show $ S.size (produceSand rocks) - S.size rocks

part2 :: S.Set Coordinate -> String
part2 = undefined
