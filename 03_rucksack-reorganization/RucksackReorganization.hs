module RucksackReorganization (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.List (intersect)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Text.Megaparsec (endBy, some)
import Text.Megaparsec.Char (letterChar, newline)

type Item = Char

type Rucksack = [Item]

parseInput :: Parser [Rucksack]
parseInput = some letterChar `endBy` newline

compartments :: Rucksack -> ([Item], [Item])
compartments = splitAt =<< (`div` 2) . length

commonItem :: [Item] -> [Item] -> Item
commonItem xs ys = head $ intersect xs ys

priority :: Item -> Int
priority item = fromJust . lookup item $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]

groups :: [Rucksack] -> [[Rucksack]]
groups = chunksOf 3

badgeItem :: [Rucksack] -> Item
badgeItem = head . foldr1 intersect

part1 :: [Rucksack] -> String
part1 = show . sum . map (priority . uncurry commonItem . compartments)

part2 :: [Rucksack] -> String
part2 = show . sum . map (priority . badgeItem) . groups
