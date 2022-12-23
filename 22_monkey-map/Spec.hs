module Main (main) where

import AdventOfCode (aocTest)
import MonkeyMap (parseInput, part1, part2)

main :: IO ()
main = aocTest "22_monkey-map" parseInput (part1, "106094") (part2, "162038")
