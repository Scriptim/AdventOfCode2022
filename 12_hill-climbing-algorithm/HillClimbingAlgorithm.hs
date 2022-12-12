{-# LANGUAGE TupleSections #-}

module HillClimbingAlgorithm (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (ord)
import Data.List.Extra (minimumOn, delete, intersect)
import qualified Data.Map as M
import Text.Megaparsec (endBy, some)
import Text.Megaparsec.Char (letterChar, newline)

type Coordinate = (Int, Int)

type HeightMap = M.Map Coordinate Char

parseInput :: Parser (HeightMap, Coordinate, Coordinate)
parseInput = extractPositions <$> heightMap
  where
    heightMap = buildMap . zip [0 ..] <$> (zip [0 ..] <$> some letterChar) `endBy` newline
    buildMap = concatMap (\(rowNum, row) -> map (first (rowNum,)) row)
    findCoordinate x = fst . head . filter ((== x) . snd)
    extractPositions heightMap' =
      let position = findCoordinate 'S' heightMap'
          bestSignal = findCoordinate 'E' heightMap'
          fullHeightMap = M.insert bestSignal 'z' . M.insert position 'a' $ M.fromAscList heightMap'
       in (fullHeightMap, position, bestSignal)

dijkstra :: HeightMap -> Maybe Coordinate -> [Coordinate] -> M.Map Coordinate Int -> M.Map Coordinate Int
dijkstra _ _ [] dist = dist
dijkstra heightMap shortCircuitTarget queue dist
  | Just current == shortCircuitTarget = dist
  | otherwise = dijkstra heightMap shortCircuitTarget (delete current queue) (foldl handleNeighbor dist neighbors)
  where
    current@(row, col) = minimumOn (dist M.!) queue
    neighbors = filter connected ([(succ row, col), (pred row, col), (row, succ col), (row, pred col)] `intersect` queue)
    connected (row', col') = ord (heightMap M.! (row, col)) - ord (heightMap M.! (row', col')) <= 1
    handleNeighbor dist' neighbor = M.insertWith min neighbor (boundedSucc $ dist M.! current) dist'
    boundedSucc x = if x == maxBound then x else succ x

shortestPathLengths :: HeightMap -> Maybe Coordinate -> Coordinate -> M.Map Coordinate Int
shortestPathLengths heightMap shortCircuitTarget source = dijkstra heightMap shortCircuitTarget initQueue initDist
  where
    initQueue = M.keys heightMap
    initDist = M.insert source 0 . M.fromAscList $ zip (M.keys heightMap) (repeat maxBound)

shortestFullPathLength :: HeightMap -> Coordinate -> Int
shortestFullPathLength heightMap source = minimum $ map (pathLengths M.!) startPoints
  where
    startPoints = M.keys $ M.filter (=='a') heightMap
    pathLengths = shortestPathLengths heightMap Nothing source

part1 :: (HeightMap, Coordinate, Coordinate) -> String
part1 (heightMap, position, bestSignal) = show $ shortestPathLengths heightMap (Just position) bestSignal M.! position

part2 :: (HeightMap, Coordinate, Coordinate) -> String
part2 (heightMap, _, bestSignal) = show $ shortestFullPathLength heightMap bestSignal
