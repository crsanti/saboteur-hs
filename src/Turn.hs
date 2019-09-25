module Turn where

import Data.Maybe

import Card
import Game
import Board
import Utils
import Player

data PlayerAction = PlayActionCard ActionCard String
                  | PlayRockFall Coord
                  | PlayPathCard PathCard Coord
                  | PlayMapCard Coord
                  | ConfirmRevealedCard
                  | DiscardActionCard ActionCard
                  | DiscardPathCard PathCard
                  | SelectNugget GoldNuggetCard
                  deriving (Show, Eq)

getActions :: Game -> Either String [PlayerAction]
getActions g = case status g of
  GameFinished          _   -> Left "Game is already over"
  ToSelectNugget        _   -> Right $ map SelectNugget (goldDeck g)
  ToConfirmRevealedCard _ _ -> Right [ConfirmRevealedCard]
  ToPlay                p   ->
    let player            = getPlayerFromName p (players g)
        pathCardActions   = maybe [] (getPathCardActions (board g)) player
        actionCardActions = maybe [] (getActionCardActions (players g) (board g)) player
    in Right $ pathCardActions ++ actionCardActions

getPathCardActions :: Board -> Player -> [PlayerAction]
getPathCardActions _ Player{ hand = Deck [] _ } = []
getPathCardActions _ Player{ brokenTools = b } | anyBrokenTool b = []
  where
    anyBrokenTool :: BrokenTools -> Bool
    anyBrokenTool (False, False, False) = False
    anyBrokenTool _                     = True
getPathCardActions b Player{ hand = Deck cs _ } =
  [PlayPathCard c coord | coord <- getEmptyCellsFromStart b, c <- cs, canPlacePathCard b c coord]

getActionCardActions :: [Player] -> Board -> Player -> [PlayerAction]
getActionCardActions ps b Player{ hand = Deck _ cs } = concatMap getActionsForCard cs
  where
    getActionsForCard :: ActionCard -> [PlayerAction]
    getActionsForCard c@(BrokenToolCard t)          = [PlayActionCard c (name p) | p <- ps, canBreakTool t p]
    getActionsForCard c@(RepairToolCard t)          = [PlayActionCard c (name p) | p <- ps, canRepairTool t p]
    getActionsForCard c@(RepairDoubleToolCard t t') = [PlayActionCard c (name p) | p <- ps, canRepairTool t p || canRepairTool t' p]
    getActionsForCard RockFallCard                  = [PlayRockFall (x, y) | (x, y, p) <- b, p `notElem` startCard : goalCards]
    getActionsForCard MapCard                       = map (PlayMapCard . tripleToPair) $ getUnflippedGoalCells b

canBreakTool :: Tool -> Player -> Bool
canBreakTool Pickaxe Player{ brokenTools = (False, _, _)} = True
canBreakTool Lantern Player{ brokenTools = (_, False, _)} = True
canBreakTool Cart Player{ brokenTools = (_, _, False)}    = True
canBreakTool _ _ = False

canRepairTool :: Tool -> Player -> Bool
canRepairTool Pickaxe Player{ brokenTools = (True, _, _)} = True
canRepairTool Lantern Player{ brokenTools = (_, True, _)} = True
canRepairTool Cart Player{ brokenTools = (_, _, True)}    = True
canRepairTool _ _ = False

{-
PlayActionCard ActionCard String
PlayRockFall Coord
PlayPathCard PathCard Coord
PlayMapCard Coord
ConfirmRevealedCard
DiscardActionCard ActionCard
DiscardPathCard PathCard
SelectNugget GoldNuggetCard
-}
performAction :: Game -> PlayerAction -> Either String Game
performAction g@Game{ status = (ToPlay p) } (PlayMapCard cs)
  | cs `notElem` goalPositions = Left "Invalid goal card location"
  | otherwise = (\c -> g{status = ToConfirmRevealedCard p c}) <$>
  maybeToEither ("No card at position" ++ show cs) (getCardAt (board g) cs)


{-
On his turn, a player must first play a card. This means:
  * Add a path card to the maze
  * Put down an action card in front of a player,
  * Pass, putting one card face down on the discard pile.
-}

-- performAction :: PlayerAction -> Game -> Game
-- performAction (PlayerAction p _) game@Game{ status = (ToPlay p') } | p /= p' = game
-- performAction (PlayerAction p _) game@Game{ status = (ToConfirmRevealedCard p' _) } | p /= p' = game
-- performAction (PlayerAction _ (PlayRockFall x y)) game@Game{ board = b } = maybe game removePathCard (getCardAt x y b)
--   where
--     removePathCard :: PathCard -> Game
--     removePathCard (PathCard{ pathCardType = p })
--       | p == ConnectedCard || p == DeadEndCard = game{ board = removeCardAt x y b }
--       | otherwise                              = game

-- performAction action@(PlayerAction p (PlayMapCard (x, y))) game
--   | isPlayerOnTurn p game && hasPlayerActionCard p MapCard game && ((x, y)`elem` goalPositions) = playMapCard action game

-- performAction action@(PlayerAction p (ConfirmRevealedCard)) game
--   | (isPlayerOnTurn p game) = fromMaybe game (updatePlayerTurn game)

-- playMapCard action@(PlayerAction p card@(PlayMapCard (x, y))) game@Game{ board = b } = fromMaybe game $ setConfirmCard game >>= draw p
--   where
--     setConfirmCard :: p -> Maybe Game
--     setConfirmCard g = getCardAt x y b >>= \card -> return game{ status = ToConfirmRevealedCard p card }

-- updatePlayerTurn :: Game -> Maybe Game
-- updatePlayerTurn game@Game{ players = ps, status = (ToPlay p) } = getNextPlayer >>= setNextPlayer
-- updatePlayerTurn game@Game{ players = ps, status = (ToConfirmRevealedCard p _) } = getNextPlayer >>= setNextPlayer
-- updatePlayerTurn game@Game{ players = ps, status = (ToSelectNugget p) } = getNextPlayer >>= setNextPlayer
--   where
--     getNextPlayer :: Maybe Player
--     getNextPlayer = (getPlayerFromName p ps) >>= (flip nextElem) ps
--     setNextPlayer nextP = return game{ status = ToPlay (name nextP) }

isPlayerOnTurn :: String -> Game -> Bool
isPlayerOnTurn p Game{ status = ToPlay p' }                  = p == p'
isPlayerOnTurn p Game{ status = ToSelectNugget p' }          = p == p'
isPlayerOnTurn p Game{ status = ToConfirmRevealedCard p' _ } = p == p'
isPlayerOnTurn _ _                                           = False

hasPlayerActionCard :: String -> ActionCard -> Game -> Bool
hasPlayerActionCard p c Game{ players = ps } = maybe False (actionCardInHand c) (getPlayerFromName p ps)

actionCardInHand :: ActionCard -> Player -> Bool
actionCardInHand c Player{ hand = Deck _ cs } = c `elem` cs

pathCardInHand :: PathCard -> Player -> Bool
pathCardInHand c Player{ hand = Deck cs _ } = c `elem` cs


draw :: String -> Game -> Maybe Game
draw p game@Game{ players = ps, deck = Deck [] [] } = return game
draw p game@Game{ players = ps, deck = Deck [] (c:cs) } = do
  player@Player{ hand = Deck ps' as' } <- getPlayerFromName p ps
  return game { deck = Deck [] cs, players = updatePlayer player{ hand = Deck ps' (c:as') } ps }

draw p game@Game{ players = ps, deck = Deck (c:cs) [] } = do
  player@Player{ hand = Deck ps' as' } <- getPlayerFromName p ps
  return game { deck = Deck cs [], players = updatePlayer player{ hand = Deck (c:ps') as' } ps }

draw p game@Game{ players = ps, seed = s, deck = d } = do
  player <- getPlayerFromName p ps
  return undefined
