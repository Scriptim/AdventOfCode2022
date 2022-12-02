module Main (main) where

import AdventOfCode (aocBench)
import RockPaperScissors (parseInput, part1, part2)

main :: IO ()
main = aocBench "02_rock-paper-scissors" parseInput part1 part2
