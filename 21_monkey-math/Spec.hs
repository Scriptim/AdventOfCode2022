module Main (main) where

import AdventOfCode (aocTest)
import MonkeyMath (parseInput, part1, part2)

main :: IO ()
main = aocTest "21_monkey-math" parseInput (part1, "158731561459602") (part2, "3769668716709")
