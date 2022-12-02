module RockPaperScissors (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (some, (<|>))
import Text.Megaparsec.Char (char, newline)

data Shape = Rock | Paper | Scissors

parseInput :: Parser [(Shape, Shape)]
parseInput = some parsePair
  where
    parsePair = do
      opp <- shape
      _ <- char ' '
      self <- shape
      _ <- newline
      return (opp, self)
    shape = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C' <|> Rock <$ char 'X' <|> Paper <$ char 'Y' <|> Scissors <$ char 'Z'

beats :: Shape -> Shape -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Shape -> Shape -> Int
outcomeScore opp self
  | beats opp self = 0
  | beats self opp = 6
  | otherwise = 3

roundScore :: Shape -> Shape -> Int
roundScore opp self = shapeScore self + outcomeScore opp self

part1 :: [(Shape, Shape)] -> String
part1 = show . sum . map (uncurry roundScore)

part2 :: [(Shape, Shape)] -> String
part2 = undefined
