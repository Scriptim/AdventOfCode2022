{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RockPaperScissors (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Text.Megaparsec (some, (<|>))
import Text.Megaparsec.Char (char, newline, space)

data Shape = Rock | Paper | Scissors deriving (Enum, Generic, NFData)

data Outcome = Loss | Draw | Win deriving (Eq, Enum, Generic, NFData)

data ShapeOrOutcome = ShapeOrOutcome Shape Outcome deriving (Generic, NFData)

parseInput :: Parser [(Shape, ShapeOrOutcome)]
parseInput = some $ (,) <$> shape <* space <*> shapeOrOutcome <* newline
  where
    shape = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'
    shapeOrOutcome = ShapeOrOutcome Rock Loss <$ char 'X' <|> ShapeOrOutcome Paper Draw <$ char 'Y' <|> ShapeOrOutcome Scissors Win <$ char 'Z'

beats :: Shape -> Shape -> Bool
beats a b = fromEnum a == mod (fromEnum b + 1) 3

outcome :: Shape -> Shape -> Outcome
outcome opp self
  | beats opp self = Loss
  | beats self opp = Win
  | otherwise = Draw

shapeScore :: Shape -> Int
shapeScore = succ . fromEnum

outcomeScore :: Outcome -> Int
outcomeScore = (* 3) . fromEnum

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
