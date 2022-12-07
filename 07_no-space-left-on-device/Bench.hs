module Main (main) where

import AdventOfCode (aocBench)
import NoSpaceLeftOnDevice (parseInput, part1, part2)

main :: IO ()
main = aocBench "07_no-space-left-on-device" parseInput part1 part2
