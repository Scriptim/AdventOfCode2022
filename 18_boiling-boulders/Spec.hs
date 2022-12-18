module Main (main) where

import AdventOfCode (aocTest)
import BoilingBoulders (parseInput, part1, part2)

main :: IO ()
main = aocTest "18_boiling-boulders" parseInput (part1, "4580") (part2, "2610")
