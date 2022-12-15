module Main (main) where

import AdventOfCode (aocBench)
import BeaconExclusionZone (parseInput, part1, part2)

main :: IO ()
main = aocBench "15_beacon-exclusion-zone" parseInput part1 part2
