module Game (
  DeckCard (..),
  Game (..),
  Player (..),
) where

import Data.List (elemIndex)

import Card (DeckCard)
import Player (Player)


data Game = Game {
  deck :: [DeckCard],
  discardPile :: [DeckCard],
  players :: [Player],
  round :: Int,
  playerOnTurn :: Player
  }

rotatePlayer :: Game -> Either String Game
rotatePlayer game@Game{ players = ps, playerOnTurn = p }
  | null ps   = Left "no players to rotate"
  | otherwise =
    case elemIndex p ps of
      Just index -> Right $ game { playerOnTurn = nextPlayer index ps }
      Nothing -> Left "player does not exist"
    where
      nextPlayer :: Int -> [Player] -> Player
      nextPlayer i ps' = cycle ps' !! (i + 1)
