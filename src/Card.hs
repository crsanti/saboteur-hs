module Card (
  ActionCardType (..),
  DeckCard (..),
  Direction (..),
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

data PathCardType = ConnectedCard
                  | DeadEndCard
                  | StartCard
                  | GoldCard
                  | StoneCard
                  deriving (Show, Eq)

data DeckCard = ActionCard ActionCardType
              | PathCard PathCardType Bool Bool Bool Bool Bool
              deriving (Show, Eq)

flipPathCard :: DeckCard -> Either String DeckCard
flipPathCard (PathCard p n e w s i) = Right $ PathCard p s w n e i
flipPathCard _                      = Left "flipCard :: cannot flip ActionCard"


data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq)

-- canConnectPathCard :: direction to connect -> played path card -> path card to check side
canConnectPathCard :: Direction -> Maybe DeckCard -> Maybe DeckCard -> Either String Bool
canConnectPathCard d (Just p)  Nothing  = getPathCardTypeFromDirection d p
canConnectPathCard d (Just p) (Just p') = liftA2 (&&) (getPathCardTypeFromDirection d p) (getPathCardTypeFromDirection d p')
canConnectPathCard _ Nothing  _  = Left "canConnectPathCard :: Cannot use empty block as base to connect path card"

getPathCardTypeFromDirection :: Direction -> DeckCard -> Either String Bool
getPathCardTypeFromDirection North (PathCard _ north _    south _    inverted) = Right $ if inverted then south else north
getPathCardTypeFromDirection East  (PathCard _ _     east _     west inverted) = Right $ if inverted then west  else east
getPathCardTypeFromDirection South (PathCard _ north _    south _    inverted) = Right $ if inverted then north else south
getPathCardTypeFromDirection West  (PathCard _ _     east _     west inverted) = Right $ if inverted then east  else west
getPathCardTypeFromDirection _      p                                          = Left  $ "getPathCardTypeFromDirection :: Cannot get PathCardType of (" ++ show p ++ ")"

newDeck :: [DeckCard]
newDeck =
  replicate 5 (PathCard ConnectedCard True  False True  False False) ++
  replicate 5 (PathCard ConnectedCard False True  False True  False) ++
  replicate 5 (PathCard ConnectedCard True  True  False False False) ++
  replicate 5 (PathCard ConnectedCard True  False False True  False) ++
  replicate 5 (PathCard ConnectedCard True  False True  True  False) ++
  replicate 5 (PathCard ConnectedCard True  True  False True  False) ++
  replicate 5 (PathCard ConnectedCard True  True  True  True  False) ++
  [PathCard DeadEndCard True  False False False False] ++
  [PathCard DeadEndCard False True  False False False] ++
  [PathCard DeadEndCard True  True  False False False] ++
  [PathCard DeadEndCard True  False False True  False] ++
  [PathCard DeadEndCard True  False True  False False] ++
  [PathCard DeadEndCard False True  False True  False] ++
  [PathCard DeadEndCard True  True  True  False False] ++
  [PathCard DeadEndCard True  True  False True  False] ++
  [PathCard DeadEndCard True  True  True  True  False] ++
  replicate 2 (ActionCard $ RepairToolCard Pickaxe) ++
  replicate 2 (ActionCard $ RepairToolCard Lantern) ++
  replicate 2 (ActionCard $ RepairToolCard Cart) ++
  [ActionCard $ RepairDoubleToolCard Pickaxe Lantern] ++
  [ActionCard $ RepairDoubleToolCard Pickaxe Cart] ++
  [ActionCard $ RepairDoubleToolCard Lantern Cart] ++
  replicate 3 (ActionCard $ BrokenToolCard Pickaxe) ++
  replicate 3 (ActionCard $ BrokenToolCard Lantern) ++
  replicate 3 (ActionCard $ BrokenToolCard Cart) ++
  replicate 3 (ActionCard RockFallCard) ++
  replicate 7 (ActionCard MapCard)
