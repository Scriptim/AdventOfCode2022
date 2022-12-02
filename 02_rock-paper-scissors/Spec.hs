module Main (main) where

import AdventOfCode (aocTest)
import RockPaperScissors (parseInput, part1, part2)

main :: IO ()
main = aocTest "02_rock-paper-scissors" parseInput (part1, "14531") (part2, "11258")
