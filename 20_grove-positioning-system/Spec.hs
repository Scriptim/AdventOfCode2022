module Main (main) where

import AdventOfCode (aocTest)
import GrovePositioningSystem (parseInput, part1, part2)

main :: IO ()
main = aocTest "20_grove-positioning-system" parseInput (part1, "6640") (part2, "11893839037215")
