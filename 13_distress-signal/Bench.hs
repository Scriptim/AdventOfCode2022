module Main (main) where

import AdventOfCode (aocBench)
import DistressSignal (parseInput, part1, part2)

main :: IO ()
main = aocBench "13_distress-signal" parseInput part1 part2
