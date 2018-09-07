module Lib(
  module Board,
  module Card,
  module Game,
  module Player,
  module State,
  module Utils,
  -- createGame,
  showGameDetails,
) where

import Control.Monad
import Control.Monad.State
import System.Random.Shuffle

import Board
import Card
import Game
import Player
import State
import Utils

-- createGame :: [String] -> IO ()
-- createGame [] = Left "At least 3 players required"
-- createGame ps = do
--   roles <- getAvailableRoles $ length ps
--   shuffled <- shuffleM roles
--   game <- setRoles shuffled ps
--   print game

-- setRoles :: [Role] -> [String] -> Either String Game
-- setRoles [] _  = Left "setRoles :: roles are empty"
-- setRoles _  [] = Left "setRoles :: players are empty"
-- setRoles rs ps = Right $ newGame { players = zipWith initPlayer rs ps }

showGameDetails :: StateT Game IO ()
showGameDetails = do
  game <- get
  lift $ print game

startGame :: StateT Game IO ()
startGame = undefined
