module Main (main) where

import AdventOfCode (aocBench)
import HillClimbingAlgorithm (parseInput, part1, part2)

main :: IO ()
main = aocBench "12_hill-climbing-algorithm" parseInput part1 part2
