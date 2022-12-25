module FullOfHotAir (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (swap)
import Text.Megaparsec (endBy, oneOf, some)
import Text.Megaparsec.Char (newline)

data SNAFUDigit = DoubleMinus | Minus | Zero | One | Two deriving (Eq, Enum)

type SNAFUNumber = [SNAFUDigit]

snafuChars :: [(Char, SNAFUDigit)]
snafuChars = zip "=-012" [DoubleMinus, Minus, Zero, One, Two]

instance Show SNAFUDigit where
  show digit = pure . fromJust . lookup digit . map swap $ snafuChars

parseInput :: Parser [SNAFUNumber]
parseInput = some digit `endBy` newline
  where
    digit = fromJust . flip lookup snafuChars <$> oneOf (map fst snafuChars)

numberValue :: SNAFUNumber -> Int
numberValue = sum . zipWith (*) (iterate (* length snafuChars) 1) . reverse . map (pred . pred . fromEnum)

snafuNumber :: Int -> SNAFUNumber
snafuNumber = reverse . go
  where
    go 0 = []
    go n = toEnum ((n + 2) `mod` length snafuChars) : go ((n + 2) `div` length snafuChars)

part1 :: [SNAFUNumber] -> String
part1 = concatMap show . snafuNumber . sum . map numberValue

part2 :: [SNAFUNumber] -> String
part2 = undefined
