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

type Stack = Map.Map Int [Char]

type Move = (Int, Int, Int)

topRow :: Stack -> Maybe String
topRow stack = mapM (fmap head . flip Map.lookup stack) [1 .. Map.size stack]

performWith :: Bool -> Maybe Stack -> Move -> Maybe Stack
performWith canPickMultiple wrappedStack (many, fromKey, toKey) =
  do
    stack <- wrappedStack
    fromStack <- Map.lookup fromKey stack
    toStack <- Map.lookup toKey stack
    let moveThese = take many fromStack
    return
      ( Map.insert fromKey (drop many fromStack)
          . Map.insert
            toKey
            ( (if canPickMultiple then id else reverse) moveThese
                ++ toStack
            )
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
part1 = topRow <=< uncurry (foldl $ performWith False) . parse

part2 :: String -> Maybe String
part2 = topRow <=< uncurry (foldl $ performWith True) . parse
