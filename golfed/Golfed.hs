{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.List.Split (splitOn)
import Data.List (sort)
import Data.Char (ord)

-- main :: IO ()
-- main = dayXXpartX

day01part1 = print . maximum . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day01part2 = print . sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day02part1 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in x' + 7 - 3 * ((ord a - 64 - x') `mod` 3)) . lines =<< readFile "02_rock-paper-scissors/input.txt"

day02part2 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in (ord a - 65 + x' - 1) `mod` 3 + 1 + x' * 3) . lines =<< readFile "02_rock-paper-scissors/input.txt"
