module Main (main) where

import AdventOfCode (aocBench)
import CampCleanup (parseInput, part1, part2)

main :: IO ()
main = aocBench "04_camp-cleanup" parseInput part1 part2
