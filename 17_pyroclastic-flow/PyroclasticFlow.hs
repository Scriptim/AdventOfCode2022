module PyroclasticFlow (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Applicative (liftA2)
import Data.Bifunctor (first, second)
import Data.List.Extra (maximumOn)
import qualified Data.Set as S
import Data.Tuple.Extra (snd3)
import Text.Megaparsec (some, (<|>))
import Text.Megaparsec.Char (char)

type Coordinate = (Int, Int)

type RockShape = [Coordinate]

data Movement = MoveLeft | MoveRight | MoveDown

parseInput :: Parser [Movement]
parseInput = cycle <$> some (MoveLeft <$ char '<' <|> MoveRight <$ char '>')

shapes :: [RockShape]
shapes =
  cycle
    [ [(2, 0), (3, 0), (4, 0), (5, 0)],
      [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)],
      [(2, 0), (3, 0), (4, 0), (4, 1), (4, 2)],
      [(2, 0), (2, 1), (2, 2), (2, 3)],
      [(2, 0), (3, 0), (2, 1), (3, 1)]
    ]

move :: Movement -> RockShape -> RockShape
move MoveLeft = map $ first pred
move MoveRight = map $ first succ
move MoveDown = map $ second pred

fall :: (S.Set Coordinate, Int, [Movement]) -> RockShape -> (S.Set Coordinate, Int, [Movement])
fall (_, _, []) _ = undefined
fall (stopped, top, movement : movements) rock
  | landed = (foldr S.insert stopped nextPosV, max top (snd $ maximumOn snd nextPosV), movements)
  | otherwise = fall (stopped, top, movements) nextPosV
  where
    moveH = move movement rock
    nextPosH = if collided then rock else moveH
    collided = any (\(x, y) -> x < 0 || x >= 7 || S.member (x, y) stopped) moveH
    moveV = move MoveDown nextPosH
    nextPosV = if landed then nextPosH else moveV
    landed = any (\(x, y) -> y < 0 || S.member (x, y) stopped) moveV

produceRocks :: [RockShape] -> [Movement] -> (S.Set Coordinate, Int, [Movement])
produceRocks rocks movements = foldl (liftA2 (.) fall moveUp) (S.empty, -1, movements) rocks
  where
    moveUp (_, top, _) = map (second (+ (top + 4)))

part1 :: [Movement] -> String
part1 = show . succ . snd3 . produceRocks (take 2022 shapes)

part2 :: [Movement] -> String
part2 = undefined
