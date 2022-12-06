module TuningTrouble (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (letterChar)

parseInput :: Parser [Char]
parseInput = some letterChar

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x : xs) = notElem x xs && distinct xs

marker :: Int -> [Char] -> Int
marker n xs
  | distinct (take n xs) = n
  | otherwise = 1 + marker n (tail xs)

part1 :: [Char] -> String
part1 = show . marker 4

part2 :: [Char] -> String
part2 = show . marker 14
