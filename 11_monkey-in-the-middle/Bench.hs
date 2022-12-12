module Main (main) where

import AdventOfCode (aocBench)
import MonkeyInTheMiddle (parseInput, part1, part2)

main :: IO ()
main = aocBench "11_monkey-in-the-middle" parseInput part1 part2
