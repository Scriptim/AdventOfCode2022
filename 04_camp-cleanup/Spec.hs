module Main (main) where

import AdventOfCode (aocTest)
import CampCleanup (parseInput, part1, part2)

main :: IO ()
main = aocTest "04_camp-cleanup" parseInput (part1, "466") (part2, "865")
