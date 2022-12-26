module Main (main) where

import AdventOfCode (aocBench)
import BlizzardBasin (parseInput, part1, part2)

main :: IO ()
main = aocBench "24_blizzard-basin" parseInput part1 part2
