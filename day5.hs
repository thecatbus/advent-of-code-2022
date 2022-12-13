module Day5 where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map qualified as Map
import System.IO

main = do
  fileContents <- openFile "Data/day5.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

data Machine = M9000 | M9001 deriving (Eq)

type Stack = Map.Map Int [Char]

type Move = (Int, Int, Int)

topRow :: Stack -> Maybe String
topRow stack = mapM (fmap head . flip Map.lookup stack) [1 .. Map.size stack]

performWith :: Machine -> Maybe Stack -> Move -> Maybe Stack
performWith machine maybeStack (many, fromKey, toKey) =
  do
    stack <- maybeStack
    fromStack <- Map.lookup fromKey stack
    toStack <- Map.lookup toKey stack
    let movedBlock =
          (if machine == M9001 then id else reverse) $
            take many fromStack
    return
      ( Map.insert fromKey (drop many fromStack)
          . Map.insert toKey (movedBlock ++ toStack)
          $ stack
      )

parse :: String -> (Maybe Stack, [Move])
parse = bimap parseStack (map parseMove) . sliceOn "" . lines
  where
    sliceOn a = second (drop 1) . span (/= a)
    parseStack =
      Just
        . Map.fromList
        . map (\xs -> (read [last xs], xs))
        . filter (/= "")
        . map (dropWhile (`elem` "[ ]"))
        . transpose
    parseMove =
      (\[a, b, c] -> (a, b, c))
        . map read
        . filter (not . any isLetter)
        . words

part1 :: String -> Maybe String
part1 = topRow <=< uncurry (foldl $ performWith M9000) . parse

part2 :: String -> Maybe String
part2 = topRow <=< uncurry (foldl $ performWith M9001) . parse
