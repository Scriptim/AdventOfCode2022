module Main (main) where

import AdventOfCode (aocBench)
import TreetopTreeHouse (parseInput, part1, part2)

main :: IO ()
main = aocBench "08_treetop-tree-house" parseInput part1 part2
