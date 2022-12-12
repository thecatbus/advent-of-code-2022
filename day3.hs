module Day3 where

import Control.Monad
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day3.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

priorities :: [(Char, Int)]
priorities = zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1, 2 ..]

priorityOf :: Char -> Maybe Int
priorityOf = flip lookup priorities

part1 :: String -> Maybe Int
part1 = fmap sum . mapM (priorityOf . sharedItem) . lines
  where
    sharedItem list = head $ uncurry intersect $ splitAt (length list `div` 2) list

part2 :: String -> Maybe Int
part2 = fmap sum . mapM (priorityOf . badge) . triples . lines
  where
    triples [] = []
    triples (a : b : c : ds) = (a, b, c) : triples ds
    badge (a, b, c) = head $ a `intersect` b `intersect` c
