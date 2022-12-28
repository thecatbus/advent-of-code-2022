module Day8 where

import Control.Monad
import Data.Function
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day8.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

fromAllSides :: ([a] -> [b]) -> [[a]] -> [[[b]]]
fromAllSides fromLeft =
  map transpose
    . transpose
    . sequence
      [ map fromLeft,
        map (reverse . fromLeft . reverse),
        transpose . map fromLeft . transpose,
        transpose . map (reverse . fromLeft . reverse) . transpose
      ]

visibilityMap :: String -> [[Bool]]
visibilityMap = map (map or) . fromAllSides visFromLeft . lines
  where
    visFromLeft = concatMap markHead . groupBy (>=)
    markHead xs = True : (False <$ tail xs)

scenicMap :: String -> [[Int]]
scenicMap = map (map product) . fromAllSides scenesFromLeft . lines
  where
    scenesFromLeft = zipWith visRange <*> (drop 1 . tails)
    visRange x xs = min (1 + length (takeWhile (< x) xs)) (length xs)

part1 :: String -> Int
part1 = length . filter (== True) . concat . visibilityMap

part2 :: String -> Int
part2 = maximum . concat . scenicMap
