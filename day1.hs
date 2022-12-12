module Day1 where

import Control.Monad
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day1.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

reFormat :: String -> String
reFormat = unwords . map (\x -> if x == "" then "\n" else x) . lines

allCalories :: String -> [Int]
allCalories = map (sum . map read . words) . lines . reFormat

part1 :: String -> Int
part1 = maximum . allCalories

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . allCalories
