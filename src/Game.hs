module Game (
  Game (..),
  GameState (..),
  PlayerAction (..),
  drawActionCard,
  drawPathCard,
  getPlayerFromName,
  newGame,
  performAction,
  performAction,
  playActionCardOverPlayer,
  removePlayedActionCard,
  replace,
  updatePlayer,
) where

import Data.List (find, delete)

import Card
import Player
import Board
import Utils

data GameState = ToAddPlayers
               | ToStartGame
               | ToPlay String
               | RoundFinished
               | ToSelectNugget String [GoldNuggetCard]
               | GameFinished
               deriving (Show)

data PlayerActionType = PlayActionCard ActionCard (Maybe String)
                      | PlayPathCard PathCard
                      | DiscardActionCard ActionCard
                      | DiscardPathCard PathCard
                      | SelectNugget GoldNuggetCard
                      deriving (Show)

data PlayerAction = PlayerAction String PlayerActionType
                    deriving (Show)

data Game = Game {
  deck :: Deck,
  players :: [Player],
  gameRound :: Int,
  gameState :: GameState
  } deriving (Show)


newGame :: Game
newGame = Game {
  deck       = newDeck,
  players    = [],
  gameRound  = 0,
  gameState  = ToAddPlayers
}

getPlayerFromName :: String -> Game -> Either String Player
getPlayerFromName target game@Game{players = ps} = maybe (Left "getPlayerFromName :: player does not exist") Right foundPlayer
  where
    foundPlayer = find (\p -> name p == target) ps

removePlayedActionCard :: ActionCard -> Player -> Either String Player
removePlayedActionCard card player@Player{ hand = (pathCards, actionCards)}
  | card `elem` actionCards = Right $ player { hand = (pathCards, delete card actionCards)}
removePlayedActionCard card player@Player{name = n} = Left $ n ++ " does not have card " ++ show card

playActionCardOverPlayer :: ActionCard -> Player -> Either String Player
playActionCardOverPlayer (BrokenToolCard tool) player@Player { brokenTools = bts }
  | tool `notElem` bts = Right player { brokenTools = tool:bts }
playActionCardOverPlayer (RepairToolCard tool) player@Player { brokenTools = bts }
  | tool `elem` bts = Right player { brokenTools = delete tool bts }
-- TODO: Give player option to choose what tool to repair in case both tools are broken
playActionCardOverPlayer (RepairDoubleToolCard tool _) player@Player { brokenTools = bts }
  | tool `elem` bts = Right player { brokenTools = delete tool bts }
playActionCardOverPlayer (RepairDoubleToolCard _ tool) player@Player { brokenTools = bts }
  | tool `elem` bts = Right player { brokenTools = delete tool bts }
playeActionCardOverPlayer card player = Left $ "playActionCardOverPlayer :: cannot play card " ++ show card ++ " on player " ++ name player

replace :: Eq a => a -> a -> [a] -> [a]
replace _   _   []        = []
replace old new (next:xs)
  | next == old = new:xs
  | otherwise   = next:replace old new xs

updatePlayer :: Player -> Game -> Either String Game
updatePlayer player game@Game{ players = ps } = case find hasSameName ps of
  Just player' -> Right $ game { players = replace player' player ps }
  Nothing      -> Left  $ name player ++ " is not a player"
  where
    hasSameName :: Player -> Bool
    hasSameName player'' = name player == name player''

drawActionCard :: [ActionCard] -> Player -> Player
drawActionCard []           player = player
drawActionCard (card:cards) player = let (pathCards, actionCards) = hand player
                                     in player { hand = (pathCards, card:actionCards)}

drawPathCard :: [PathCard] -> Player -> Either String Player
drawPathCard []           player = return player
drawPathCard (card:cards) player = return player

performAction :: PlayerAction -> Game -> Either String Game
performAction
  action@(PlayerAction playerWhoPlays (PlayActionCard card (Just target)))
  game@Game{ gameState = (ToPlay playerOnTurn), deck = (_, actionCards)}
  | playerWhoPlays == playerOnTurn = do
    targetPlayer <- getPlayerFromName target game >>= playActionCardOverPlayer card
    playerOnTurn <- drawActionCard actionCards <$> getPlayerFromName playerWhoPlays game >>= removePlayedActionCard card
    updatePlayer targetPlayer game >>= updatePlayer playerOnTurn

performAction action game  = Left $ "performAction :: invalid action" ++ show action ++ " for state " ++  show (gameState game)
