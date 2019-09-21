module Player where

import Card
import System.Random
import Data.List(find)

import Utils(shuffleList)

data Role = Miner
          | Saboteur
          deriving (Show, Eq)

type Hand = Deck
-- BrokenTools :: (Pickaxe, Lantern, Cart)
type BrokenTools = (Bool, Bool, Bool)

data Player = Player {
  hand :: Hand,
  name :: String,
  playerRole :: Role,
  goldNuggets :: [GoldNuggetCard],
  brokenTools :: BrokenTools
} deriving (Show, Eq)

newPlayer :: Role -> String -> Player
newPlayer r n = Player{ name = n, playerRole = r, hand = Deck [] [], goldNuggets = [], brokenTools = (False, False, False) }

setupPlayers :: StdGen -> [String] -> [Player]
setupPlayers s ps =
  let rs = shuffleList s $ getNeededRoles (length ps)
  in zipWith newPlayer rs ps
  where
    getNeededRoles :: Int -> [Role]
    getNeededRoles  3 = Saboteur : replicate 3 Miner
    getNeededRoles  4 = Miner    : getNeededRoles 3
    getNeededRoles  5 = Saboteur : getNeededRoles 4
    getNeededRoles  6 = Miner    : getNeededRoles 5
    getNeededRoles  7 = Saboteur : getNeededRoles 6
    getNeededRoles  8 = Miner    : getNeededRoles 7
    getNeededRoles  9 = Miner    : getNeededRoles 8
    getNeededRoles 10 = Saboteur : getNeededRoles 9

updatePlayer :: Player -> [Player] -> [Player]
updatePlayer p = map (replacePlayer p)
  where
    replacePlayer :: Player -> Player -> Player
    replacePlayer p@Player{ name = n } p'@Player{ name = n' }
      | n == n'   = p
      | otherwise = p'

getPlayerFromName :: String -> [Player] -> Maybe Player
getPlayerFromName p = find (\Player{ name = n } -> n == p)
