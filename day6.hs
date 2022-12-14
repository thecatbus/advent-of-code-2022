module Day6 where

import Control.Monad
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day6.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

part1 :: String -> Int
part1 = (4 +) . length . takeWhile (< 4) . map (length . nub . take 4) . tails

part2 :: String -> Int
part2 = (14 +) . length . takeWhile (< 14) . map (length . nub . take 14) . tails
