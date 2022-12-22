module GrovePositioningSystem (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parseInput :: Parser [Int]
parseInput = signed (pure ()) decimal `endBy` newline

move :: S.Seq (Int, Int) -> Int -> S.Seq (Int, Int)
move list n = S.insertAt newIndex element . S.deleteAt index $ list
  where
    index = fromJust $ S.findIndexL ((== n) . fst) list
    element@(_, value) = S.index list index
    newIndex = (index + value) `mod` pred (S.length list)

mix :: S.Seq Int -> S.Seq Int
mix xs = snd . S.unzip $ foldl move (S.zip (S.fromList indices) xs) indices
  where
    indices = [0 .. S.length xs - 1]

groveCoordinatesSum :: S.Seq Int -> Int
groveCoordinatesSum list = nth 1000 + nth 2000 + nth 3000
  where
    zero = fromJust $ S.findIndexL (== 0) list
    nth n = S.index list $ (zero + n) `mod` S.length list

part1 :: [Int] -> String
part1 = show . groveCoordinatesSum . mix . S.fromList

part2 :: [Int] -> String
part2 = undefined
