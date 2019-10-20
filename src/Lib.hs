module Lib where

import System.Random(StdGen)
import Data.Either
import Game
import Turn
import Card
import Board
import Mocks
import Utils
import PrettyPrint

newGame :: [String] -> StdGen -> Either String Game
newGame ps gen
  | validPlayersLength ps = return $ initGame ps gen
  | otherwise = fail "You must provide a valid number of players from 3 to 10 players"
  where
    validPlayersLength :: [a] -> Bool
    validPlayersLength ps = (playersLength >= 3) && playersLength <= 10
    playersLength :: Int
    playersLength = length ps

play :: PlayerAction -> Game -> Either String Game
play a g | a `elem` getActions g = performAction a g
         | otherwise             = Left "Invalid action"
