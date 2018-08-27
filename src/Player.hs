module Player (
  Player (..),
  PType (..),
  initPlayer
) where

import Card (DeckCard, Tool)

data PType = Dwarf
           | Saboteur
           deriving (Show, Eq)

data Player = Player {
  hand :: [DeckCard],
  name :: String,
  pType :: PType,
  goldNuggets :: Int,
  brokenTools :: [Tool]
} deriving (Show, Eq)

initPlayer :: PType -> String -> Player
initPlayer pt n = Player { name = n, pType = pt, hand = [], goldNuggets = 0, brokenTools = [] }
