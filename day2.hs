module Day2 where

import Control.Monad
import Data.List
import System.IO

main = do
  fileContents <- openFile "Data/day2.txt" ReadMode
  input <- hGetContents fileContents
  print $ part1 input
  print $ part2 input
  hClose fileContents

data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Result = Win | Loss | Draw deriving (Show, Eq)

data Game = Game
  { theirMove :: Move,
    ourMove :: Move,
    result :: Result,
    moveScore :: Int,
    resultScore :: Int
  }
  deriving (Show)

score :: Game -> Int
score game = moveScore game + resultScore game

allGames =
  let moves = [Rock, Paper, Scissors]
      mScores = [1, 2, 3]
   in map (\(m1, m2, res, mscore, rscore) -> Game m1 m2 res mscore rscore) $
        zip5 moves moves (repeat Draw) (cycle mScores) (repeat 3)
          ++ zip5 moves (drop 1 $ cycle moves) (repeat Win) (drop 1 $ cycle mScores) (repeat 6)
          ++ zip5 moves (drop 2 $ cycle moves) (repeat Loss) (drop 2 $ cycle mScores) (repeat 0)

gameFromMoves :: (Move, Move) -> Maybe Game
gameFromMoves =
  flip lookup $
    map (\game -> ((theirMove game, ourMove game), game)) allGames

gameFromTheirmoveAndResult :: (Move, Result) -> Maybe Game
gameFromTheirmoveAndResult =
  flip lookup $
    map (\game -> ((theirMove game, result game), game)) allGames

part1 :: String -> Maybe Int
part1 = fmap sum . mapM (fmap score . readGame) . lines
  where
    readGame = gameFromMoves . (\[m1, m2] -> (m1, m2)) . map readMove . words
    readMove x
      | x `elem` ["A", "X"] = Rock
      | x `elem` ["B", "Y"] = Paper
      | x `elem` ["C", "Z"] = Scissors
      | otherwise = error "Cannot read moves"

part2 :: String -> Maybe Int
part2 = fmap sum . mapM (fmap score . readGame) . lines
  where
    readGame = gameFromTheirmoveAndResult . (\[m, r] -> (readMove m, readResult r)) . words
    readMove x
      | x == "A" = Rock
      | x == "B" = Paper
      | x == "C" = Scissors
      | otherwise = error "Cannot read move"
    readResult x
      | x == "X" = Loss
      | x == "Y" = Draw
      | x == "Z" = Win
      | otherwise = error "Cannot read result"
