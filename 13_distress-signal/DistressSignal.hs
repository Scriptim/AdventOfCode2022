{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module DistressSignal (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Control.DeepSeq (NFData)
import Data.List (sort)
import GHC.Generics (Generic)
import Text.Megaparsec (between, sepBy, (<|>))
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Packet = PacketList [Packet] | PacketValue Int deriving (Eq, Generic, NFData)

parseInput :: Parser [(Packet, Packet)]
parseInput = packetPair `sepBy` newline
  where
    packetPair = (,) <$> packet <* newline <*> packet <* newline
    packet = packetList <|> packetValue
    packetList = PacketList <$> between (char '[') (char ']') (packet `sepBy` char ',')
    packetValue = PacketValue <$> decimal

instance Ord Packet where
  compare (PacketValue x) (PacketValue y) = compare x y
  compare (PacketList xs) (PacketList ys) = compare xs ys
  compare (PacketValue x) (PacketList ys) = compare (PacketList [PacketValue x]) (PacketList ys)
  compare (PacketList xs) (PacketValue y) = compare (PacketList xs) (PacketList [PacketValue y])

dividers :: [Packet]
dividers = [PacketList [PacketList [PacketValue 2]], PacketList [PacketList [PacketValue 6]]]

part1 :: [(Packet, Packet)] -> String
part1 = show . sum . map fst . filter (uncurry (<=) . snd) . zip [(1 :: Int) ..]

part2 :: [(Packet, Packet)] -> String
part2 = show . product . map fst . filter ((`elem` dividers) . snd) . zip [(1 :: Int) ..] . sort . (dividers ++) . concatMap (\(a, b) -> [a, b])
