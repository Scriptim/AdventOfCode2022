module Main (main) where

import AdventOfCode (aocBench)
import RucksackReorganization (parseInput, part1, part2)

main :: IO ()
main = aocBench "03_rucksack-reorganization" parseInput part1 part2
