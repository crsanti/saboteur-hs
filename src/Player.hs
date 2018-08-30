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

data Hand = Hand [PathCard] [ActionCard]
          deriving (Show, Eq)

data Player = Player {
  hand :: Hand,
  name :: String,
  playerRole :: Role,
  goldNuggets :: Int,
  brokenTools :: [Tool]
} deriving (Show, Eq)

initPlayer :: Role -> String -> Player
initPlayer pt n = Player { name = n, playerRole = pt, hand = Hand [] [], goldNuggets = 0, brokenTools = [] }
