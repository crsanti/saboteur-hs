module Card (
  ActionCard (..),
  ActionCardType (..),
  Deck (..),
  Direction (..),
  PathCard (..),
  PathCardType (..),
  Tool (..),
  canConnectPathCard,
  flipPathCard,
  getPathCardTypeFromDirection,
  newDeck,
) where

import Control.Applicative (liftA2)

data Tool = Pickaxe
          | Lantern
          | Cart
          deriving (Show, Eq)

data ActionCardType = BrokenToolCard Tool
                    | RepairToolCard Tool
                    | RepairDoubleToolCard Tool Tool
                    | RockFallCard
                    | MapCard
                      deriving (Show, Eq)

newtype ActionCard = ActionCard ActionCardType
                   deriving (Show, Eq)

data PathCardType = ConnectedCard
                  | DeadEndCard
                  | StartCard
                  | GoldCard
                  | StoneCard
                  deriving (Show, Eq)

data PathCard = PathCard {
  pathCardType :: PathCardType,
  north :: Bool,
  east :: Bool,
  south :: Bool,
  west :: Bool,
  inverted :: Bool
} deriving (Show, Eq)

data Deck = Deck [PathCard] [ActionCard]

flipPathCard :: PathCard -> PathCard
flipPathCard PathCard{ pathCardType = p, north = n, east = e, south = s, west = w, inverted = i } =
  PathCard{ pathCardType = p, north = s, east = w, south = n, west = e, inverted = not i }

data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq)

-- | canConnectPathCard :: direction to connect -> played path card -> path card to check side
canConnectPathCard :: Direction -> Maybe PathCard -> Maybe PathCard -> Either String Bool
canConnectPathCard d (Just p)  Nothing  = Right $ getPathCardTypeFromDirection d p
canConnectPathCard d (Just p) (Just p') = Right $ getPathCardTypeFromDirection d p && getPathCardTypeFromDirection (invertDirection d) p'
canConnectPathCard _  Nothing  _        = Left "canConnectPathCard :: Cannot use empty block as base to connect path card"

invertDirection :: Direction -> Direction
invertDirection North = South
invertDirection East  = West
invertDirection South = North
invertDirection West  = East

getPathCardTypeFromDirection :: Direction -> PathCard -> Bool
getPathCardTypeFromDirection North p = if inverted p then south p else north p
getPathCardTypeFromDirection East  p = if inverted p then west  p  else east p
getPathCardTypeFromDirection South p = if inverted p then north p else south p
getPathCardTypeFromDirection West  p = if inverted p then east  p  else west p

newDeck = Deck initPathCards initActionCards
  where
    initPathCards :: [PathCard]
    initPathCards =
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True,  inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = True,  inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  inverted = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True,  inverted = False }) ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = False, inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = False, inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = False, inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = True,  inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = True,  west = False, inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = True,  inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = False, inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = True,  inverted = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = True,  inverted = False }]
    initActionCards :: [ActionCard]
    initActionCards =
      replicate 2 (ActionCard $ RepairToolCard Pickaxe) ++
      replicate 2 (ActionCard $ RepairToolCard Lantern) ++
      replicate 2 (ActionCard $ RepairToolCard Cart) ++
      replicate 3 (ActionCard $ BrokenToolCard Pickaxe) ++
      replicate 3 (ActionCard $ BrokenToolCard Lantern) ++
      replicate 3 (ActionCard $ BrokenToolCard Cart) ++
      replicate 3 (ActionCard RockFallCard) ++
      replicate 7 (ActionCard MapCard) ++
      [ActionCard $ RepairDoubleToolCard Pickaxe Lantern] ++
      [ActionCard $ RepairDoubleToolCard Pickaxe Cart] ++
      [ActionCard $ RepairDoubleToolCard Lantern Cart]
