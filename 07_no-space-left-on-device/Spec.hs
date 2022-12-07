module Main (main) where

import AdventOfCode (aocTest)
import NoSpaceLeftOnDevice (parseInput, part1, part2)

main :: IO ()
main = aocTest "07_no-space-left-on-device" parseInput (part1, "2031851") (part2, "2568781")
