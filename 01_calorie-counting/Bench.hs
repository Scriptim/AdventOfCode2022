module Main (main) where

import AdventOfCode (aocBench)
import CalorieCounting (parseInput, part1, part2)

main :: IO ()
main = aocBench "01_calorie-counting" parseInput part1 part2
