module Main (main) where

import AdventOfCode (aocBench)
import TuningTrouble (parseInput, part1, part2)

main :: IO ()
main = aocBench "06_tuning-trouble" parseInput part1 part2
