{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Char (ord)
import Data.List (elemIndex, intersect, sort)
import Data.List.Split (splitOn, chunksOf)

-- main :: IO ()
-- main = dayXXpartX

day01part1 = print . maximum . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day01part2 = print . sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day02part1 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in x' + 7 - 3 * ((ord a - 64 - x') `mod` 3)) . lines =<< readFile "02_rock-paper-scissors/input.txt"

day02part2 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in (ord a - 65 + x' - 1) `mod` 3 + 1 + x' * 3) . lines =<< readFile "02_rock-paper-scissors/input.txt"

day03part1 = print . sum . map (maybe 0 succ . (`elemIndex` (['a'..'z']++['A'..])) . head . uncurry intersect . (splitAt =<< (`div` 2) . length)) . lines =<< readFile "03_rucksack-reorganization/input.txt"

day03part2 = print . sum . map (maybe 0 succ . (`elemIndex` (['a' .. 'z'] ++ ['A' ..])) . head . foldr1 intersect) . chunksOf 3 . lines =<< readFile "03_rucksack-reorganization/input.txt"
