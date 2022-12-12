module Day4 where

import Control.Monad
import Data.Bifunctor
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day4.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

type Range = (Int, Int)

readLine :: String -> (Range, Range)
readLine = join bimap readRange . sliceOn ','
  where
    readRange = join bimap read . sliceOn '-'
    sliceOn a = second (drop 1) . span (/= a)

part1 :: String -> Int
part1 = length . filter fullOverlap . map readLine . lines
  where
    fullOverlap (xs, ys) = ys `contains` xs || xs `contains` ys
    contains (x1, x2) (y1, y2) = (y1 >= x1) && (y2 <= x2)

part2 :: String -> Int
part2 = length . filter someOverlap . map readLine . lines
  where
    someOverlap (xs, ys) = ys `overshoots` xs || xs `overshoots` ys
    overshoots (x1, x2) (y1, y2) = x2 >= y1 && x1 <= y1
