module BeaconExclusionZone (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Monad (guard)
import Data.Text (pack)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Coordinate = (Int, Int)

type Sensor = (Coordinate, Coordinate)

parseInput :: Parser [Sensor]
parseInput = sensor `endBy` newline
  where
    sensor = (,) <$> (string (pack "Sensor at ") *> coordinatePair) <*> (string (pack ": closest beacon is at ") *> coordinatePair)
    coordinatePair = (,) <$> coordinate 'x' <* string (pack ", ") <*> coordinate 'y'
    coordinate axis = string (pack $ axis : "=") *> signed (return ()) decimal :: Parser Int

distance :: Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

beaconable :: [Sensor] -> Coordinate -> Bool
beaconable sensors coord = all (\(sensor, beacon) -> coord == beacon || distance sensor coord > distance sensor beacon) sensors

row :: [Sensor] -> Int -> [Coordinate]
row sensors y = [(x, y) | x <- [(leftSensor - maxSensorRange) .. (rightSensor + maxSensorRange)]]
  where
    maxSensorRange = maximum . map (uncurry distance) $ sensors
    leftSensor = minimum . map (fst . fst) $ sensors
    rightSensor = maximum . map (fst . fst) $ sensors

sensorCircle :: Sensor -> [Coordinate]
sensorCircle (sensor@(x, y), beacon) = do
  let radius = succ $ distance sensor beacon
  xDelta <- [-radius .. radius]
  yDelta <- [abs xDelta - radius, radius - abs xDelta]
  return (x + xDelta, y + yDelta)

undetected :: [Sensor] -> [Coordinate]
undetected sensors = do
  coord@(x, y) <- concatMap sensorCircle sensors
  guard $ x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
  guard $ beaconable sensors coord
  guard $ coord `notElem` map fst sensors
  return coord

part1 :: [Sensor] -> String
part1 sensors = show . length . filter (not . beaconable sensors) $ row sensors 2000000

part2 :: [Sensor] -> String
part2 = show . (\(x, y) -> x * 4000000 + y) . head . undetected
