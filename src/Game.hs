module Game where

import System.Random

import Card
import Player (Player(..), initPlayers)
import Board
import Utils
import Data.List

data GameStatus = ToStartGame
                | ToAddPlayers
                | ToPlay String
                | ToConfirmRevealedCard String PathCard
                | RoundFinished
                | ToSelectNugget String
                | GameFinished
                deriving (Show)

data Game = Game {
  deck :: Deck,
  players :: [Player],
  gameRound :: Int,
  status :: GameStatus,
  seed :: StdGen,
  board :: Board,
  goldDeck :: [GoldNuggetCard]
}

instance Show Game where
  show game = "{\n\
    \  deck: "      ++ show (deck game)      ++ "\n\
    \  players: "   ++ show (players game)   ++ "\n\
    \  gameRound: " ++ show (gameRound game) ++ "\n\
    \  status: "    ++ show (status game)    ++ "\n\
    \  seed: "      ++ show (seed game)      ++ "\n\
    \  board: "     ++ show (board game)     ++ "\n\
    \  goldDeck: "  ++ show (goldDeck game)  ++ "\n\
    \}"

initGame :: [String] -> StdGen -> Game
initGame ps s = dealStartingCards $ newGame ps s
  where
    newGame :: [String] -> StdGen -> Game
    newGame ps s = Game {
      deck      = shuffleDeck s newDeck,
      players   = initPlayers s ps,
      gameRound = 1,
      status    = ToAddPlayers,
      seed      = s,
      board     = initBoard s,
      goldDeck  = shuffleList s newGoldenDeck
    }

dealStartingCards :: Game -> Game
dealStartingCards game = dealStartingCards' (numberOfPlayers game) game
  where
    dealStartingCards' :: Int -> Game -> Game
    dealStartingCards' 0 game = game
    dealStartingCards' pos game@Game{ players = ps } =
      let (xs, s)  = getRandomProbs (numberOfPlayers game) (seed game)
          p        = ps !! (pos - 1)
          (d, p') = dealCardsToPlayer xs (deck game) p
      in dealStartingCards' (pos - 1) game{ seed = s, deck = d, players = (replaceNth (pos - 1) p' (players game)) }
    numberOfPlayers :: Game -> Int
    numberOfPlayers = length . players
    dealCardsToPlayer :: [Int] -> Deck -> Player -> (Deck, Player)
    dealCardsToPlayer [] d p = (d, p)
    dealCardsToPlayer (x:xs) (Deck (p:ps) (a:as)) player@Player{ hand = (Deck playerPathCards playerActionCards) }
      | x > 6     = dealCardsToPlayer xs (Deck (p:ps) as) player { hand = (Deck playerPathCards (a:playerActionCards)) }
      | otherwise = dealCardsToPlayer xs (Deck ps (a:as)) player { hand = (Deck (p:playerPathCards) playerActionCards) }


-- getPlayerFromName :: String -> Game -> Either String Player
-- getPlayerFromName target game@Game{players = ps} = maybe (Left "getPlayerFromName :: player does not exist") Right foundPlayer
--   where
--     foundPlayer = find (\p -> name p == target) ps

-- removePlayedActionCard :: ActionCard -> Player -> Context Player
-- removePlayedActionCard card player@Player{ hand = (pathCards, actionCards)}
--   | card `elem` actionCards = return player { hand = (pathCards, delete card actionCards)}
-- removePlayedActionCard card player@Player{name = n} = liftEither .  Left $ n ++ " does not have card " ++ show card

-- playActionCardOverPlayer :: ActionCard -> Player -> Either String Player
-- playActionCardOverPlayer (BrokenToolCard tool) player@Player { brokenTools = bts }
--   | tool `notElem` bts = Right player { brokenTools = tool:bts }
-- playActionCardOverPlayer (RepairToolCard tool) player@Player { brokenTools = bts }
--   | tool `elem` bts = Right player { brokenTools = delete tool bts }
-- -- TODO: Give player option to choose what tool to repair in case both tools are broken
-- playActionCardOverPlayer (RepairDoubleToolCard tool _) player@Player { brokenTools = bts }
--   | tool `elem` bts = Right player { brokenTools = delete tool bts }
-- playActionCardOverPlayer (RepairDoubleToolCard _ tool) player@Player { brokenTools = bts }
--   | tool `elem` bts = Right player { brokenTools = delete tool bts }
-- playeActionCardOverPlayer card player = Left $ "playActionCardOverPlayer :: cannot play card " ++ show card ++ " on player " ++ name player

-- replace :: Eq a => a -> a -> [a] -> [a]
-- replace _   _   []        = []
-- replace old new (next:xs)
--   | next == old = new:xs
--   | otherwise   = next:replace old new xs

-- updatePlayer :: Player -> Game -> Either String Game
-- updatePlayer player game@Game{ players = ps } = case find hasSameName ps of
--   Just player' -> Right $ game { players = replace player' player ps }
--   Nothing      -> Left  $ name player ++ " is not a player"
--   where
--     hasSameName :: Player -> Bool
--     hasSameName player'' = name player == name player''

-- drawActionCard :: [ActionCard] -> Player -> Player
-- drawActionCard []           player = player
-- drawActionCard (card:cards) player = let (pathCards, actionCards) = hand player
--                                      in player { hand = (pathCards, card:actionCards)}

-- drawPathCard :: [PathCard] -> Player -> Either String Player
-- drawPathCard []           player = return player
-- drawPathCard (card:cards) player = return player

-- performAction :: PlayerAction -> Game -> Either String Game
-- performAction action@(PlayerAction playerWhoPlays _) game@Game{ gameState = (ToPlay playerOnTurn) }
--  | playerWhoPlays /= playerOnTurn = Left $ "performAction :: Invalid action for player " ++ playerWhoPlays ++ ". It's not on turn"
-- performAction
--   action@(PlayerAction _ (PlayActionCard card (Just target)))
--   game@Game{ gameState = (ToPlay playerOnTurn), deck = (_, actionCards)} = do
--     targetPlayer <- getPlayerFromName target game >>= playActionCardOverPlayer card
--     undefined
--     -- playerOnTurn <- drawActionCard actionCards <$> getPlayerFromName playerWhoPlays game >>= removePlayedActionCard card
--     -- updatePlayer targetPlayer game >>= updatePlayer playerOnTurn

-- performAction action game  = Left $ "performAction :: invalid action" ++ show action ++ " for state " ++  show (gameState game)
