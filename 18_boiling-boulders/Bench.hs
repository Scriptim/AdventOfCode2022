module Main (main) where

import AdventOfCode (aocBench)
import BoilingBoulders (parseInput, part1, part2)

main :: IO ()
main = aocBench "18_boiling-boulders" parseInput part1 part2
