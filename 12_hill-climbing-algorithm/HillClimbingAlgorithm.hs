{-# LANGUAGE TupleSections #-}

module HillClimbingAlgorithm (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (ord)
import Data.List.Extra (minimumOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (uncurry3)
import Text.Megaparsec (endBy, some)
import Text.Megaparsec.Char (letterChar, newline)

type Coordinate = (Int, Int)

type HeightMap = M.Map Coordinate Char

parseInput :: Parser (HeightMap, Coordinate, Coordinate)
parseInput = extractPositions <$> heightMap
  where
    heightMap = buildMap . zip [0 ..] <$> (zip [0 ..] <$> some letterChar) `endBy` newline
    buildMap = concatMap (\(rowNum, row) -> map (first (rowNum,)) row)
    position x = fst . head . filter ((== x) . snd)
    extractPositions heightMap' =
      let start = position 'S' heightMap'
          goal = position 'E' heightMap'
          fullHeightMap = M.insert goal 'z' . M.insert start 'a' $ M.fromAscList heightMap'
       in (fullHeightMap, start, goal)

shortestPathLength :: HeightMap -> Coordinate -> Coordinate -> Int
shortestPathLength heightMap start goal = go initOpen initPathLengths initEstimates
  where
    maxBoundMap = M.fromAscList $ zip (M.keys heightMap) (repeat maxBound)
    heuristic node = abs (fst node - fst goal) + abs (snd node - snd goal)
    initOpen = S.singleton start
    initPathLengths = M.insert start 0 maxBoundMap
    initEstimates = M.insert start (heuristic start) maxBoundMap
    go open pathLengths estimates
      | S.null open = error "no path found"
      | current == goal = pathLengths M.! goal
      | otherwise = uncurry3 go $ foldl handleNeighbor (S.delete current open, pathLengths, estimates) neighbors
      where
        current@(row, col) = minimumOn (estimates M.!) (S.toList open)
        neighbors = filter (liftA2 (&&) valid reachable) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
        valid = flip M.member heightMap
        reachable (row', col') = ord (heightMap M.! (row', col')) - ord (heightMap M.! (row, col)) <= 1
        handleNeighbor prevState@(open', pathLengths', estimates') neighbor
          | score >= pathLengths' M.! neighbor = prevState
          | otherwise = (S.insert neighbor open', M.insert neighbor score pathLengths', M.insert neighbor (score + heuristic neighbor) estimates')
          where
            score = succ $ pathLengths' M.! current

part1 :: (HeightMap, Coordinate, Coordinate) -> String
part1 (heightMap, start, goal) = show $ shortestPathLength heightMap start goal

part2 :: (HeightMap, Coordinate, Coordinate) -> String
part2 = undefined
