module RockPaperScissors (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (some, (<|>))
import Text.Megaparsec.Char (char, newline, space)

data Shape = Rock | Paper | Scissors

data Outcome = Loss | Draw | Win deriving (Eq)

data ShapeOrOutcome = ShapeOrOutcome Shape Outcome

parseInput :: Parser [(Shape, ShapeOrOutcome)]
parseInput = some parseRound
  where
    parseRound = do
      opp <- shape
      _ <- space
      self <- shapeOrOutcome
      _ <- newline
      return (opp, self)
    shape = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'
    shapeOrOutcome = ShapeOrOutcome Rock Loss <$ char 'X' <|> ShapeOrOutcome Paper Draw <$ char 'Y' <|> ShapeOrOutcome Scissors Win <$ char 'Z'

beats :: Shape -> Shape -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

outcome :: Shape -> Shape -> Outcome
outcome opp self
  | beats opp self = Loss
  | beats self opp = Win
  | otherwise = Draw

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Loss = 0
outcomeScore Draw = 3
outcomeScore Win = 6

action :: Shape -> Outcome -> Shape
action opp outc = head $ filter ((==) outc . outcome opp) [Rock, Paper, Scissors]

roundScore1 :: Shape -> ShapeOrOutcome -> Int
roundScore1 opp (ShapeOrOutcome self _) = shapeScore self + outcomeScore (outcome opp self)

roundScore2 :: Shape -> ShapeOrOutcome -> Int
roundScore2 opp (ShapeOrOutcome _ outc) = shapeScore (action opp outc) + outcomeScore outc

part1 :: [(Shape, ShapeOrOutcome)] -> String
part1 = show . sum . map (uncurry roundScore1)

part2 :: [(Shape, ShapeOrOutcome)] -> String
part2 = show . sum . map (uncurry roundScore2)
