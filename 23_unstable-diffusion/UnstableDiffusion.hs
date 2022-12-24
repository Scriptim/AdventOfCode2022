{-# LANGUAGE TupleSections #-}

module UnstableDiffusion (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Applicative (liftA2)
import Data.List.Extra (find)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (both)
import Text.Megaparsec (endBy, some, (<|>))
import Text.Megaparsec.Char (char, newline)

type Coordinate = (Int, Int)

type Board = S.Set Coordinate

data Direction = North | South | West | East deriving (Enum, Bounded)

parseInput :: Parser Board
parseInput = S.fromList . map fst . filter snd . addCoords <$> board
  where
    addCoords = concat . zipWith (\row -> zipWith (\col -> ((row, col),)) [0 ..]) [0 ..]
    board = some (False <$ char '.' <|> True <$ char '#') `endBy` newline

adjacent :: Coordinate -> Direction -> [Coordinate]
adjacent (row, col) North = [(pred row, col), (pred row, pred col), (pred row, succ col)]
adjacent (row, col) South = [(succ row, col), (succ row, pred col), (succ row, succ col)]
adjacent (row, col) West = [(row, pred col), (pred row, pred col), (succ row, pred col)]
adjacent (row, col) East = [(row, succ col), (pred row, succ col), (succ row, succ col)]

directionsCycle :: [[Direction]]
directionsCycle = iterate (liftA2 (++) tail (take 1)) [minBound .. maxBound]

proposedBoard :: Board -> [Direction] -> M.Map Coordinate Coordinate
proposedBoard board directions = M.fromSet proposeOrStay board
  where
    hasNeighbor current = not . S.null $ S.fromList (concatMap (adjacent current) [minBound .. maxBound]) `S.intersection` board
    propose current = maybe current head . find (all (`S.notMember` board)) . map (adjacent current) $ directions
    proposeOrStay current = if hasNeighbor current then propose current else current

performMoves :: M.Map Coordinate Coordinate -> Board
performMoves proposedBoard' = S.fromList . M.elems . M.mapWithKey destination $ proposedBoard'
  where
    destination current proposed = if collisionFree proposed then proposed else current
    collisionFree pos = (<= 1) . length . filter (== Just pos) . map (proposedBoard' M.!?) $ neighbors pos
    neighbors (row, col) = [(pred row, col), (row, succ col), (succ row, col), (row, pred col)]

boardStates :: Board -> [Board]
boardStates initBoard = scanl ((performMoves .) . proposedBoard) initBoard directionsCycle

emptyTiles :: Board -> Int
emptyTiles = liftA2 (-) regionSize S.size
  where
    regionSize = uncurry (*) . both (succ . abs . uncurry (-)) . bounds ((maxBound, minBound), (maxBound, minBound)) . S.toList
    bounds accu [] = accu
    bounds ((top, bottom), (left, right)) ((row, col) : rest) = bounds ((min top row, max bottom row), (min left col, max right col)) rest

part1 :: Board -> String
part1 = show . emptyTiles . (!! 10) . boardStates

part2 :: Board -> String
part2 = undefined
