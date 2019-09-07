module Lib where

import Text.Printf(printf)
import System.Random(StdGen)
import Game
import Turn
import Card
import Board
import Mocks

newGame :: [String] -> StdGen -> Either String Game
newGame ps gen
  | validPlayersLength ps = return $ initGame ps gen
  | otherwise = fail "You must provide a valid number of players from 3 to 10 players"
  where
    validPlayersLength :: [a] -> Bool
    validPlayersLength ps = (playersLength >= 3) && playersLength <= 10
    playersLength :: Int
    playersLength = length ps

play :: Game -> PlayerAction -> Either Game String
play = undefined

getActions :: Game -> Either String [PlayerAction]
getActions Game{ gameRound = r } | r `notElem` [1..3] = Left "Invalid round"
getActions Game{ status = ToSelectNugget p, goldDeck = gs } = return $ map SelectNugget gs
getActions g@Game{ status = ToPlay _ }  = getPlayActions g
