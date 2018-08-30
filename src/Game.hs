module Game (
  Game (..),
  Player (..),
) where

import Data.List (elemIndex)

import Card (Deck)
import Player (Player)


data Game = Game {
  deck :: Deck,
  discardPile :: Deck,
  players :: [Player],
  round :: Int,
  playerOnTurn :: Player
  }

rotatePlayer :: Game -> Either String Game
rotatePlayer Game{ players = [] } = Left "rotatePlayer :: no players to rotate"
rotatePlayer game@Game{ players = ps, playerOnTurn = p } =
  case elemIndex p ps of
    Just i  -> Right $ game { playerOnTurn = nextPlayer i ps }
    Nothing -> Left  $ "rotatePlayer :: Player " ++ show p ++ " does not exist"
  where
    nextPlayer :: Int -> [Player] -> Player
    nextPlayer i ps' = cycle ps' !! (i + 1)
