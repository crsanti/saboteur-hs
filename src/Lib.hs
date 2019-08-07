module Lib where

import System.Random(StdGen)
import Game (Game, initGame)
import Turn (PlayerAction)

newGame :: [String] -> StdGen -> Either String Game
newGame ps gen
  | validPlayersLength ps = return $ initGame ps gen
  | otherwise = Left "This game can only be players from 3 to 10 players"
  where
    validPlayersLength :: [a] -> Bool
    validPlayersLength ps = (playersLength >= 3) && playersLength <= 10
    playersLength :: Int
    playersLength = length ps

play :: Game -> PlayerAction -> Either Game String
play = undefined
