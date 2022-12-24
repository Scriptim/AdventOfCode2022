module Main (main) where

import AdventOfCode (aocBench)
import UnstableDiffusion (parseInput, part1, part2)

main :: IO ()
main = aocBench "23_unstable-diffusion" parseInput part1 part2
