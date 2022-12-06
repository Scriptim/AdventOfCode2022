{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Char (ord)
import Data.List (elemIndex, intersect, nub, sort, transpose)
import Data.List.Extra (trim)
import Data.List.Split (chunksOf, splitOn)

-- main :: IO ()
-- main = dayXXpartX

day01part1 = print . maximum . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day01part2 = print . sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines =<< readFile "01_calorie-counting/input.txt"

day02part1 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in x' + 7 - 3 * ((ord a - 64 - x') `mod` 3)) . lines =<< readFile "02_rock-paper-scissors/input.txt"

day02part2 = print . sum . map (\[a,_,x] -> let x' = ord x - 88 in (ord a - 65 + x' - 1) `mod` 3 + 1 + x' * 3) . lines =<< readFile "02_rock-paper-scissors/input.txt"

day03part1 = print . sum . map (maybe 0 succ . (`elemIndex` (['a'..'z']++['A'..])) . head . uncurry intersect . (splitAt =<< (`div` 2) . length)) . lines =<< readFile "03_rucksack-reorganization/input.txt"

day03part2 = print . sum . map (maybe 0 succ . (`elemIndex` (['a' .. 'z'] ++ ['A' ..])) . head . foldr1 intersect) . chunksOf 3 . lines =<< readFile "03_rucksack-reorganization/input.txt"

day04part1 = print . length . filter (\[[a,b],[c,d]] -> a<=c&&b>=d||c<=a&&d>=b) . map (map (map (read::String->Int) . splitOn "-") . splitOn ",") . lines =<< readFile "04_camp-cleanup/input.txt"

day04part2 = print . length . filter (\[[a,b],[c,d]] -> b>=c&&a<=d||d>=a&&c<=b) . map (map (map (read::String -> Int) . splitOn "-") . splitOn ",") . lines =<< readFile "04_camp-cleanup/input.txt"

day05part1 = putStrLn . map head . uncurry (foldl (\s (n,f,t) -> let a f n xs = take n xs ++ f (xs !! n) : drop (n + 1) xs in a (drop n) f $ a ((reverse . take n $ s !! f) ++) t s)) . (\[a,b] -> (map trim . transpose . map (map (!!1) . chunksOf 4) . init $ a, map ((\[_,n,_,f,_,t] -> (read n, pred $ read f, pred $ read t)) . splitOn " ") b)) . map lines . splitOn "\n\n" =<< readFile "05_supply-stacks/input.txt"

day05part2 = putStrLn . map head . uncurry (foldl (\s (n,f,t) -> let a f n xs = take n xs ++ f (xs !! n) : drop (n + 1) xs in a (drop n) f $ a ((take n $ s !! f) ++) t s)) . (\[a,b] -> (map trim . transpose . map (map (!!1) . chunksOf 4) . init $ a, map ((\[_,n,_,f,_,t] -> (read n, pred $ read f, pred $ read t)) . splitOn " ") b)) . map lines . splitOn "\n\n" =<< readFile "05_supply-stacks/input.txt"

day06part1 = print . (let m x = if length (nub $ take 4 x) == 4 then 4 else 1 + m (tail x) in m) =<< readFile "06_tuning-trouble/input.txt"

day06part2 = print . (let m x = if length (nub $ take 14 x) == 14 then 14 else 1 + m (tail x) in m) =<< readFile "06_tuning-trouble/input.txt"
