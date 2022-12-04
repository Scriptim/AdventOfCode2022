{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.List.Split (splitOn)
import Data.List (sort)

-- main :: IO ()
-- main = dayXXpartX

day01part1 = print . maximum . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day01part2 = print . sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"
