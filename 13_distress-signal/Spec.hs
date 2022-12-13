module Main (main) where

import AdventOfCode (aocTest)
import DistressSignal (parseInput, part1, part2)

main :: IO ()
main = aocTest "13_distress-signal" parseInput (part1, "5557") (part2, "22425")
