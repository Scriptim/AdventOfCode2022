module Main (main) where

import AdventOfCode (aocBench)
import SupplyStacks (parseInput, part1, part2)

main :: IO ()
main = aocBench "05_supply-stacks" parseInput part1 part2
