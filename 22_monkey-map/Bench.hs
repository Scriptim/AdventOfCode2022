module Main (main) where

import AdventOfCode (aocBench)
import MonkeyMap (parseInput, part1, part2)

main :: IO ()
main = aocBench "22_monkey-map" parseInput part1 part2
