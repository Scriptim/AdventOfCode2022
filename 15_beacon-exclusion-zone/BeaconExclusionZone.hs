module BeaconExclusionZone (parseInput, part1, part2) where

import AdventOfCode (Parser)
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

part1 :: [Sensor] -> String
part1 sensors = show . length . filter (not . beaconable sensors) $ row sensors 2000000

part2 :: [Sensor] -> String
part2 = undefined
