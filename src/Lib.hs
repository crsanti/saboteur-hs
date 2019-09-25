module Lib where

import Text.Printf(printf)
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

play :: Game -> String -> PlayerAction -> Either String Game
play g p a | canPlayAction = performAction g a
           | otherwise     = Left "Invalid action"
  where
    canPlayAction :: Bool
    canPlayAction = either (const False) (a `elem`) (getActions g)

getActionsForPlayer :: String -> Game -> Either String [PlayerAction]
getActionsForPlayer p g
  | not (isPlayer p g)       = Left $ "Player " ++ p ++ " is not a player"
  | not (isPlayerOnTurn p g) = Right []
  | otherwise                = getActions g
