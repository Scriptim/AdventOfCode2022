module NoSpaceLeftOnDevice (parseInput, part1, part2) where

import AdventOfCode (Parser)
import qualified Data.Map as M
import Data.Text (pack)
import Text.Megaparsec (endBy, some, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Filename = String

type Filesize = Int

type Path = [Filename]

data Command = CD Filename | LS [OutputLine]

data OutputLine = FileLine Filesize Filename | DirectoryLine Filename

data FileTree = File Filesize | Directory (M.Map Filename FileTree)

parseInput :: Parser [Command]
parseInput = some command
  where
    filename = some (letterChar <|> char '.')
    file = FileLine <$> decimal <* space <*> filename
    dirname = some letterChar <|> ".." <$ string (pack "..") <|> "/" <$ string (pack "/")
    dir = DirectoryLine <$> (string (pack "dir ") *> dirname)
    cd = CD <$> (string (pack "cd ") *> dirname <* newline)
    ls = LS <$> (string (pack "ls") *> newline *> (file <|> dir) `endBy` newline)
    command = string (pack "$ ") *> (cd <|> ls)

size :: FileTree -> Filesize
size (File filesize) = filesize
size (Directory children) = sum $ map size (M.elems children)

dirs :: FileTree -> [FileTree]
dirs (File _) = []
dirs dir@(Directory children) = dir : concatMap dirs (M.elems children)

insert :: Path -> FileTree -> FileTree -> FileTree
insert _ _ (File _) = undefined
insert [] _ _ = undefined
insert [filename] new (Directory children) = Directory $ M.insert filename new children
insert (next : rest) new (Directory children) = Directory $ M.adjust (insert rest new) next children

buildTree :: [Command] -> FileTree
buildTree = go (Directory M.empty) []
  where
    go tree _ [] = tree
    go tree cwd (cmd : cmds) = case cmd of
      CD "/" -> go tree [] cmds
      CD ".." -> go tree (init cwd) cmds
      CD dir -> go tree (cwd ++ [dir]) cmds
      LS children -> go (foldl insertNode tree children) cwd cmds
        where
          insertNode accu (FileLine filesize name) = insert (cwd ++ [name]) (File filesize) accu
          insertNode accu (DirectoryLine name) = insert (cwd ++ [name]) (Directory M.empty) accu

part1 :: [Command] -> String
part1 = show . sum . filter (<= 100000) . map size . dirs . buildTree

part2 :: [Command] -> String
part2 = undefined
