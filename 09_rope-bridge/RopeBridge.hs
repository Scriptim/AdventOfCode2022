{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RopeBridge (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import Data.List (nub)
import GHC.Generics (Generic)
import Text.Megaparsec (endBy, (<|>))
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Coordinate = (Int, Int)

data Direction = U | R | D | L deriving (Generic, NFData)

data Movement = Move Direction Int deriving (Generic, NFData)

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

step :: [Coordinate] -> Direction -> [Coordinate]
step [] _ = []
step (headPos : rope) dir = scanl pullTail (move headPos dir) rope

traceTail :: Int -> [Movement] -> [Coordinate]
traceTail ropeLength = map last . scanl step (replicate ropeLength (0, 0)) . concatMap chop
  where
    chop (Move dir n) = replicate n dir

part1 :: [Movement] -> String
part1 = show . length . nub . traceTail 2

part2 :: [Movement] -> String
part2 = show . length . nub . traceTail 10
