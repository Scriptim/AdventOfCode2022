module RopeBridge (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.List (nub)
import Text.Megaparsec (endBy, (<|>))
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Coordinate = (Int, Int)

data Direction = U | R | D | L

data Movement = Move Direction Int

parseInput :: Parser [Movement]
parseInput = (Move <$> direction <* space <*> decimal) `endBy` newline
  where
    direction = U <$ char 'U' <|> R <$ char 'R' <|> D <$ char 'D' <|> L <$ char 'L'

move :: Coordinate -> Direction -> Coordinate
move (x, y) U = (x, succ y)
move (x, y) R = (succ x, y)
move (x, y) D = (x, pred y)
move (x, y) L = (pred x, y)

pullTail :: Coordinate -> Coordinate -> Coordinate
pullTail (hx, hy) tailPos@(tx, ty)
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = tailPos
  | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))

step :: (Coordinate, Coordinate) -> Direction -> (Coordinate, Coordinate)
step (headPos, tailPos) dir = (headPos', tailPos')
  where
    headPos' = move headPos dir
    tailPos' = pullTail headPos' tailPos

part1 :: [Movement] -> String
part1 = show . length . nub . map snd . scanl step ((0, 0), (0, 0)) . concatMap chop
  where
    chop (Move dir n) = replicate n dir

part2 :: [Movement] -> String
part2 = undefined
