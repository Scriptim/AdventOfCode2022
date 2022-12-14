module Main (main) where

import AdventOfCode (aocBench)
import RegolithReservoir (parseInput, part1, part2)

main :: IO ()
main = aocBench "14_regolith-reservoir" parseInput part1 part2
