module Main (main) where

import AdventOfCode (aocBench)
import CathodeRayTube (parseInput, part1, part2)

main :: IO ()
main = aocBench "10_cathode-ray-tube" parseInput part1 part2
