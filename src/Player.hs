module Player (
  Hand (..),
  Player (..),
  Role (..),
  initPlayer
) where

import Card (ActionCard, PathCard, Tool)

data Role = Miner
          | Saboteur
          deriving (Show, Eq)

type Hand = ([PathCard],[ActionCard])

data Player = Player {
  hand :: Hand,
  name :: String,
  playerRole :: Role,
  goldNuggets :: Int,
  brokenTools :: [Tool]
} deriving (Show, Eq)

initPlayer :: Role -> String -> Player
initPlayer r n = Player { name = n, playerRole = r, hand = ([], []), goldNuggets = 0, brokenTools = [] }
