module Day7 where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.IO

type Path = [String]

data File = File
  { name :: String,
    location :: Path,
    size :: Int
  }
  deriving (Show, Eq)

main = do
  fileContents <- openFile "Data/day7.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

updateFileSystem :: (Path, [Path], [File]) -> String -> (Path, [Path], [File])
updateFileSystem (head, dirs, files) termLine =
  case termLine of
    "$ ls" -> (head, dirs, files)
    "$ cd .." -> (drop 1 head, dirs, files)
    "$ cd /" -> (["/"], dirs, files)
    '$' : ' ' : 'c' : 'd' : ' ' : dir -> (dir : head, dirs, files)
    'd' : 'i' : 'r' : ' ' : dir ->
      let newDir = dir : head
       in (head, addIfNew newDir dirs, files)
    _ ->
      let (size, ' ' : name) = span isDigit termLine
          newFile = File name head (read size)
       in (head, dirs, addIfNew newFile files)
  where
    addIfNew x xs = if x `notElem` xs then x : xs else xs

dirSizes :: String -> [Int]
dirSizes =
  map (sum . map (size . snd))
    . groupBy ((==) `on` fst)
    . filter (\(dir, file) -> dir `isSuffixOf` location file)
    . (\(_, allDirs, allFiles) -> (,) <$> allDirs <*> allFiles)
    . foldl updateFileSystem ([], [["/"]], [])
    . lines

part1 :: String -> Int
part1 = sum . filter (<= 100000) . dirSizes

part2 :: String -> Int
part2 = minimum . (filter =<< removable) . dirSizes
  where
    removable xs x = maximum xs - x <= 70000000 - 30000000
