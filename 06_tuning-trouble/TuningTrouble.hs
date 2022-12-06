module TuningTrouble (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (letterChar)

parseInput :: Parser [Char]
parseInput = some letterChar

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x : xs) = notElem x xs && distinct xs

marker :: [Char] -> Int
marker xs
  | distinct (take 4 xs) = 4
  | otherwise = 1 + marker (tail xs)

part1 :: [Char] -> String
part1 = show . marker

part2 :: [Char] -> String
part2 = undefined
