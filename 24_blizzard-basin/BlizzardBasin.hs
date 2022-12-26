module BlizzardBasin (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Applicative (many)
import Data.Bifunctor (bimap)
import Data.List.Extra (minimumOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (uncurry3)
import Text.Megaparsec (endBy, some, try, (<|>))
import Text.Megaparsec.Char (char, newline)

type Coordinate = (Int, Int)

data Direction = North | East | South | West

data Valley = Valley
  { height :: Int,
    width :: Int,
    blizzards :: M.Map Coordinate [Direction],
    entry :: Coordinate,
    exit :: Coordinate
  }

parseInput :: Parser Valley
parseInput = buildValley <$> wallHole <*> try (wall *> some ground <* wall) `endBy` newline <*> wallHole
  where
    buildValley entryCol valleyMap exitCol = Valley (length valleyMap) (length . head $ valleyMap) (M.fromList . withCoords $ valleyMap) (-1, entryCol) (length valleyMap, exitCol)
    withCoords = concat . zipWith (\rowI -> zipWith (\colI tile -> ((rowI, colI), tile)) [0 ..]) [0 ..]
    wallHole = length <$> (wall *> many wall) <* clearGround <* some wall <* newline
    clearGround = char '.'
    ground = pure <$> direction <|> [] <$ clearGround
    direction = North <$ char '^' <|> South <$ char 'v' <|> West <$ char '<' <|> East <$ char '>'
    wall = char '#'

move :: Coordinate -> Direction -> Coordinate
move (row, col) North = (pred row, col)
move (row, col) East = (row, succ col)
move (row, col) South = (succ row, col)
move (row, col) West = (row, pred col)

moveCyclic :: Valley -> Coordinate -> Direction -> Coordinate
moveCyclic valley pos = bimap (`mod` height valley) (`mod` width valley) . move pos

moveBlizzards :: Valley -> Valley
moveBlizzards valley = valley {blizzards = M.mapWithKey incomingBlizzards (blizzards valley)}
  where
    incomingBlizzards tile _ = concatMap (incoming tile) (neighbors tile)
    incoming tile neighbor = filter ((== tile) . moveCyclic valley neighbor) (blizzards valley M.! neighbor)
    neighbors = flip map [North, East, South, West] . moveCyclic valley

findPathLength :: Valley -> Coordinate -> [Coordinate] -> Int
findPathLength _ _ [] = 0
findPathLength valley start (stop : stops) = go initOpen initPathLengths initEstimates
  where
    initOpen = S.singleton (start, 0)
    initPathLengths = M.singleton (start, 0) 0
    initEstimates = M.singleton (start, 0) (heuristic start)
    states = iterate moveBlizzards valley
    heuristic (row, col) = abs (row - fst stop) + abs (col - snd stop)
    go open pathLengths estimates
      | S.null open = error "no path found"
      | pos == stop = iteration + findPathLength (states !! iteration) pos stops
      | otherwise = uncurry3 go $ foldl handleNode (S.delete current open, pathLengths, estimates) nextNodes
      where
        current@(pos, iteration) = minimumOn (estimates M.!) (S.toList open)
        neighbors = filter isGround . map (move pos) $ [North, East, South, West]
        isGround coord = coord == entry valley || coord == exit valley || inBounds coord
        inBounds (row, col) = row >= 0 && row < height valley && col >= 0 && col < width valley
        nextState = states !! succ iteration
        nextNodes = zip (filter (null . (($ blizzards nextState) . M.findWithDefault [])) $ pos : neighbors) (repeat (succ iteration))
        handleNode prevState@(open', pathLengths', estimates') node
          | score >= M.findWithDefault maxBound node pathLengths' = prevState
          | otherwise = (S.insert node open', M.insert node score pathLengths', M.insert node (score + heuristic (fst node)) estimates')
          where
            score = succ $ pathLengths' M.! current

findFullPathLength :: Valley -> [Coordinate] -> Int
findFullPathLength valley = findPathLength valley $ entry valley

part1 :: Valley -> String
part1 valley = show . findFullPathLength valley $ [exit valley]

part2 :: Valley -> String
part2 valley = show . findFullPathLength valley $ [exit valley, entry valley, exit valley]
