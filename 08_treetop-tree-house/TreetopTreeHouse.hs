module TreetopTreeHouse (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Monad (guard)
import Data.List (singleton)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Text.Megaparsec (endBy, some)
import Text.Megaparsec.Char (digitChar, newline)

type TreeMap = M.Matrix Int

parseInput :: Parser TreeMap
parseInput = M.fromLists <$> some (read . singleton <$> digitChar) `endBy` newline

viewingDirections :: TreeMap -> [((Int, Int), V.Vector Int, V.Vector Int, V.Vector Int, V.Vector Int)]
viewingDirections trees = do
  row <- [1 .. M.nrows trees]
  col <- [1 .. M.ncols trees]

  let rowV = M.getRow row trees
      colV = M.getCol col trees
      up = V.take (row - 1) colV
      right = V.drop col rowV
      down = V.drop row colV
      left = V.take (col - 1) rowV

  return ((row, col), up, right, down, left)

visible :: TreeMap -> [(Int, Int)]
visible trees = do
  (center, up, right, down, left) <- viewingDirections trees
  let height = trees M.! center
  guard $ V.all (< height) up || V.all (< height) right || V.all (< height) down || V.all (< height) left
  return center

takeUntil :: (a -> Bool) -> V.Vector a -> V.Vector a
takeUntil p vec = case V.findIndex p vec of
  Nothing -> vec
  Just i -> V.take (i + 1) vec

scenicScores :: TreeMap -> [Int]
scenicScores trees = do
  (center, up, right, down, left) <- viewingDirections trees
  let height = trees M.! center
      distance = V.length . takeUntil (>= height)
  return $ distance (V.reverse up) * distance right * distance down * distance (V.reverse left)

part1 :: TreeMap -> String
part1 = show . length . visible

part2 :: TreeMap -> String
part2 = show . maximum . scenicScores
