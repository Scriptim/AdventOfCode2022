module Main (main) where

import AdventOfCode (aocBench)
import GrovePositioningSystem (parseInput, part1, part2)

main :: IO ()
main = aocBench "20_grove-positioning-system" parseInput part1 part2
