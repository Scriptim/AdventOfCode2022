module MonkeyMath (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.Monad (ap, replicateM)
import qualified Data.Map as M
import Text.Megaparsec (endBy, (<|>))
import Text.Megaparsec.Char (char, lowerChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Monkey = Yell Int | Calc (Int -> Int -> Int) String String

parseInput :: Parser (M.Map String Monkey)
parseInput = M.fromList <$> line `endBy` newline
  where
    line = (,) <$> name <* char ':' <* space <*> monkey
    name = replicateM 4 lowerChar
    monkey = Yell <$> decimal <|> flip Calc <$> name <* space <*> operation <* space <*> name
    operation = (+) <$ char '+' <|> (*) <$ char '*' <|> (-) <$ char '-' <|> div <$ char '/'

yelledValue :: String -> M.Map String Monkey -> Int
yelledValue name monkeys = case monkeys M.! name of
  Yell n -> n
  Calc op nameA nameB -> op (yelledValue nameA monkeys) (yelledValue nameB monkeys)

part1 :: M.Map String Monkey -> String
part1 = show . yelledValue "root"

part2 :: M.Map String Monkey -> String
part2 = undefined
