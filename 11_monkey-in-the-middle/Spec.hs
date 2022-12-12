module Main (main) where

import AdventOfCode (aocTest)
import MonkeyInTheMiddle (parseInput, part1, part2)

main :: IO ()
main = aocTest "11_monkey-in-the-middle" parseInput (part1, "110264") (part2, "23612457316")
