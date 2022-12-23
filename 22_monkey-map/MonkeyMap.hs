{-# LANGUAGE TupleSections #-}

module MonkeyMap (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Monad (guard)
import Data.List.Extra (groupOn, maximumOn, minimumOn)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Tuple.Extra (both)
import Text.Megaparsec (endBy, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Coordinate = (Int, Int)

type Square = S.Set Coordinate

data MonkeyMap = MonkeyMap
  { squareSize :: Int,
    squares :: [(Coordinate, Square)]
  }

data Tile = Void | Open | Wall deriving (Eq)

data Movement = Forward Int | TurnLeft | TurnRight

data Direction = East | South | West | North deriving (Enum)

boardToMonkeyMap :: [[Tile]] -> MonkeyMap
boardToMonkeyMap board = MonkeyMap squareSize' (filter (not . S.null . snd) squares')
  where
    squareSize' = minimum . map (minimum . map length . groupOn (== Void)) $ board
    width = maximum (map length board) `div` squareSize'
    height = length board `div` squareSize'
    indices = [0 .. pred squareSize']
    square row col = S.fromList $ do
      tileRow <- indices
      tileCol <- indices
      guard $ (board !! (row * squareSize' + tileRow) ++ repeat Void) !! (col * squareSize' + tileCol) == Wall
      return (tileRow, tileCol)
    squares' = [((row, col), square row col) | row <- [0 .. pred height], col <- [0 .. pred width]]

parseInput :: Parser (MonkeyMap, [Movement])
parseInput = (,) <$> board <* newline <*> movements
  where
    board = boardToMonkeyMap <$> some tile `endBy` newline
    tile = Void <$ char ' ' <|> Open <$ char '.' <|> Wall <$ char '#'
    movements = some $ Forward <$> Text.Megaparsec.Char.Lexer.decimal <|> TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'

neighborTile :: Coordinate -> Direction -> Coordinate
neighborTile (row, col) East = (row, succ col)
neighborTile (row, col) South = (succ row, col)
neighborTile (row, col) West = (row, pred col)
neighborTile (row, col) North = (pred row, col)

neighborSquare :: [Coordinate] -> Coordinate -> Direction -> Coordinate
neighborSquare squareCoords (row, col) East
  | (row, succ col) `elem` squareCoords = (row, succ col)
  | otherwise = minimumOn snd . filter ((== row) . fst) $ squareCoords
neighborSquare squareCoords (row, col) South
  | (succ row, col) `elem` squareCoords = (succ row, col)
  | otherwise = minimumOn fst . filter ((== col) . snd) $ squareCoords
neighborSquare squareCoords (row, col) West
  | (row, pred col) `elem` squareCoords = (row, pred col)
  | otherwise = maximumOn snd . filter ((== row) . fst) $ squareCoords
neighborSquare squareCoords (row, col) North
  | (pred row, col) `elem` squareCoords = (pred row, col)
  | otherwise = maximumOn fst . filter ((== col) . snd) $ squareCoords

neighbor :: MonkeyMap -> Direction -> (Coordinate, Coordinate) -> (Coordinate, Coordinate)
neighbor monkeyMap dir (squareCoord, tileCoord)
  | inSquare tileNeighbor = (squareCoord, tileNeighbor)
  | otherwise = (neighborSquare (map fst . squares $ monkeyMap) squareCoord dir, both (`mod` squareSize monkeyMap) tileNeighbor)
  where
    inSquare (row, col) = row >= 0 && row < squareSize monkeyMap && col >= 0 && col < squareSize monkeyMap
    tileNeighbor = neighborTile tileCoord dir

turn :: Movement -> Direction -> Direction
turn TurnLeft = toEnum . (`mod` 4) . pred . fromEnum
turn TurnRight = toEnum . (`mod` 4) . succ . fromEnum
turn (Forward _) = id

move :: MonkeyMap -> (Coordinate, Coordinate) -> Direction -> [Movement] -> ((Coordinate, Coordinate), Direction)
move _ pos dir [] = (pos, dir)
move monkeyMap pos dir (Forward 0 : moves) = move monkeyMap pos dir moves
move monkeyMap pos dir (Forward n : moves)
  | S.member tileNeighbor square = move monkeyMap pos dir moves
  | otherwise = move monkeyMap neighbor' dir (Forward (pred n) : moves)
  where
    neighbor'@(squareNeighbor, tileNeighbor) = neighbor monkeyMap dir pos
    square = fromJust . lookup squareNeighbor . squares $ monkeyMap
move monkeyMap pos dir (turnMove : moves) = move monkeyMap pos (turn turnMove dir) moves

initPosition :: MonkeyMap -> (Coordinate, Coordinate)
initPosition = (,(0, 0)) . minimumOn snd . filter ((== 0) . fst) . map fst . squares

password :: MonkeyMap -> ((Coordinate, Coordinate), Direction) -> Int
password monkeyMap (pos, dir) = 1000 * succ fullRow + 4 * succ fullCol + fromEnum dir
  where
    fullRow = squareRow * squareSize monkeyMap + tileRow
    fullCol = squareCol * squareSize monkeyMap + tileCol
    ((squareRow, squareCol), (tileRow, tileCol)) = pos

part1 :: (MonkeyMap, [Movement]) -> String
part1 (monkeyMap, moves) = show . password monkeyMap $ move monkeyMap (initPosition monkeyMap) East moves

part2 :: (MonkeyMap, [Movement]) -> String
part2 = undefined
