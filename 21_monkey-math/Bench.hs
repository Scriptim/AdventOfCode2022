module Main (main) where

import AdventOfCode (aocBench)
import MonkeyMath (parseInput, part1, part2)

main :: IO ()
main = aocBench "21_monkey-math" parseInput part1 part2
