module Main (main) where

import AdventOfCode (aocTest)
import CathodeRayTube (parseInput, part1, part2)

main :: IO ()
main = aocTest "10_cathode-ray-tube" parseInput (part1, "14240") (part2, "\n###..#....#..#.#....#..#.###..####.#..#.\n#..#.#....#..#.#....#.#..#..#....#.#..#.\n#..#.#....#..#.#....##...###....#..####.\n###..#....#..#.#....#.#..#..#..#...#..#.\n#....#....#..#.#....#.#..#..#.#....#..#.\n#....####..##..####.#..#.###..####.#..#.\n")
