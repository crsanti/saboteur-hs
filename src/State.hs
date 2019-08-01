module State where

import Control.Monad.State
import System.Random
import System.Random.Shuffle
import Data.List

import Game
import Card
import Player
import Board
import Utils

-- type GameState = State Game


-- addPlayers :: [ Player ] -> GameState ()
-- addPlayers ps = modify $ (\game -> game { players = ps, status = ToStartGame })

-- startGame :: GameState ()
-- startGame = shuffleDeckST >> setPlayers >> setStartingPlayer >> setNextSeed

-- shuffleDeckST :: GameState ()
-- shuffleDeckST = modify $ \game@Game{ seed = s, deck = d } -> game{ deck = shuffleDeck s d }

-- setPlayers :: GameState ()
-- setPlayers = setPlayerRoles >> dealPlayerCards >> setStartingPlayer

-- setPlayerRoles :: GameState ()
-- setPlayerRoles = do
--   game <- get
--   let ps    = players game
--       roles = shuffleRoles (length ps) (seed game)
--       ps'   = setPlayersRole roles ps
--     in put game{ players = ps' }

-- shuffleRoles :: Int -> StdGen -> [Role]
-- shuffleRoles n gen = shuffleList gen (getNeededRoles n)

-- dealPlayerCards ::GameState ()
-- dealPlayerCards = modify $ \game -> dealCards (numberOfPlayers game) game
--   where
--     dealCards :: Int -> Game -> Game
--     dealCards 0 game = game
--     dealCards pos game@Game{ players = ps } =
--       let (xs, s)  = getRandomProbs (numberOfPlayers game) (seed game)
--           p        = ps !! (pos - 1)
--           (d, p') = dealCardsToPlayer xs (deck game) p
--       in dealCards (pos - 1) game{ seed = s, deck = d, players = (replaceNth (pos - 1) p' (players game)) }

-- setStartingPlayer :: GameState ()
-- setStartingPlayer = getRandomPlayer >>= setPlayerTurn >> setNextSeed
--   where
--     getRandomPlayer :: GameState Player
--     getRandomPlayer = do
--       game <- get
--       let ps  = players game
--           ps' = shuffle' ps (length ps) (seed game)
--        in return (head ps)

--     setPlayerTurn :: Player -> GameState ()
--     setPlayerTurn p = modify $ \game -> game { status = ToPlay $ (name p) }

-- setNextSeed :: GameState ()
-- setNextSeed = modify $ \game -> game { seed = snd $ next (seed game) }

