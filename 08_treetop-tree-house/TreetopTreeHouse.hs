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

visible :: TreeMap -> [(Int, Int)]
visible trees = do
  row <- [1 .. M.nrows trees]
  col <- [1 .. M.ncols trees]

  let height = M.getElem row col trees
      rowV = M.getRow row trees
      colV = M.getCol col trees
      up = V.take (row - 1) colV
      down = V.drop row colV
      left = V.take (col - 1) rowV
      right = V.drop col rowV

  guard $ V.all (< height) up || V.all (< height) down || V.all (< height) left || V.all (< height) right
  return (col, row)

part1 :: TreeMap -> String
part1 = show . length . visible

part2 :: TreeMap -> String
part2 = undefined
