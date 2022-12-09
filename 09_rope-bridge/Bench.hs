module Main (main) where

import AdventOfCode (aocBench)
import RopeBridge (parseInput, part1, part2)

main :: IO ()
main = aocBench "09_rope-bridge" parseInput part1 part2
