module Main (main) where

import AdventOfCode (aocTest)
import RopeBridge (parseInput, part1, part2)

main :: IO ()
main = aocTest "09_rope-bridge" parseInput (part1, "6311") (part2, "2482")
