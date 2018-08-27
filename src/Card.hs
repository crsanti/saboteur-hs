module Card (
  Tool (..),
  ActionCard (..),
  PathCardSide (..),
  PathCard (..),
  DeckCard (..),
  canConnectPathCard
) where

data Tool = Pickaxe
          | Lantern
          | Cart
          deriving (Show, Eq)

data ActionCard = BrokenToolCard Tool
                | RepairToolCard Tool
                | RepairDoubleToolCard Tool Tool
                | RockFallCard
                | MapCard
                  deriving (Show, Eq)

data PathCardSide = Open
                  | Broken
                  | Closed
                  deriving (Show, Eq)

data PathCard = PathCard PathCardSide PathCardSide PathCardSide PathCardSide
              | StartCard
              | GoldCard
              | StoneCard PathCardSide PathCardSide PathCardSide PathCardSide
              deriving (Show, Eq)

data DeckCard = Action ActionCard
              | Path PathCard
              deriving (Show, Eq)

flipPathCard :: PathCard -> PathCard
flipPathCard (PathCard n e s w)  = PathCard s w n e
flipPathCard (StoneCard n e s w) = PathCard s w n e
flipPathCard pathCard            = pathCard

data Direction = North
               | East
               | South
               | West
               deriving (Show)

canConnectPathCard :: PathCardSide -> PathCardSide -> Bool
canConnectPathCard Closed Closed = True
canConnectPathCard Closed _      = False
canConnectPathCard _      Closed = False
canConnectPathCard _      _      = True
