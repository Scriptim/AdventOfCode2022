module Main (main) where

import AdventOfCode (aocTest)
import HillClimbingAlgorithm (parseInput, part1, part2)

main :: IO ()
main = aocTest "12_hill-climbing-algorithm" parseInput (part1, "472") (part2, "465")
