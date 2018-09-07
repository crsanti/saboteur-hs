module Card (
  ActionCard (..),
  Deck (..),
  Direction (..),
  GoldNuggetCard,
  PathCard (..),
  PathCardType (..),
  Tool (..),
  canConnectPathCard,
  flipPathCard,
  getPathCardTypeFromDirection,
  newDeck,
) where

newtype GoldNuggetCard = GoldNuggetCard Int
                       deriving (Show)

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

data PathCardType = ConnectedCard
                  | DeadEndCard
                  | StartCard
                  | GoldCard
                  | CoalCard
                  deriving (Show, Eq)

data PathCard = PathCard {
  pathCardType :: PathCardType,
  north :: Bool,
  east :: Bool,
  south :: Bool,
  west :: Bool,
  rotated :: Bool
} deriving (Show, Eq)

type Deck = ([PathCard], [ActionCard])

flipPathCard :: PathCard -> PathCard
flipPathCard PathCard{ pathCardType = p, north = n, east = e, south = s, west = w, rotated = r } =
  PathCard{ pathCardType = p, north = s, east = w, south = n, west = e, rotated = not r }

data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq)

-- | canConnectPathCard :: direction to connect -> played path card -> path card to check side
canConnectPathCard :: Direction -> Maybe PathCard -> Maybe PathCard -> Either String Bool
canConnectPathCard d (Just p)  Nothing  = Right $ getPathCardTypeFromDirection d p
canConnectPathCard d (Just p) (Just p') = Right $ getPathCardTypeFromDirection d p && getPathCardTypeFromDirection (oppositeDirection d) p'
canConnectPathCard _  Nothing  _        = Left "canConnectPathCard :: Cannot use empty block as base to connect path card"

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East  = West
oppositeDirection South = North
oppositeDirection West  = East

getPathCardTypeFromDirection :: Direction -> PathCard -> Bool
getPathCardTypeFromDirection North p = if rotated p then south p else north p
getPathCardTypeFromDirection East  p = if rotated p then west  p  else east p
getPathCardTypeFromDirection South p = if rotated p then north p else south p
getPathCardTypeFromDirection West  p = if rotated p then east  p  else west p

newDeck :: Deck
newDeck = (initPathCards, initActionCards)
  where
    initPathCards :: [PathCard]
    initPathCards =
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True,  rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = True,  rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True,  rotated = False }) ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = True,  rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = True,  west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = True,  rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = True,  rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = True,  rotated = False }]
    initActionCards :: [ActionCard]
    initActionCards =
      replicate 2 (RepairToolCard Pickaxe) ++
      replicate 2 (RepairToolCard Lantern) ++
      replicate 2 (RepairToolCard Cart) ++
      replicate 3 (BrokenToolCard Pickaxe) ++
      replicate 3 (BrokenToolCard Lantern) ++
      replicate 3 (BrokenToolCard Cart) ++
      replicate 3 RockFallCard ++
      replicate 7 MapCard ++
      [RepairDoubleToolCard Pickaxe Lantern] ++
      [RepairDoubleToolCard Pickaxe Cart] ++
      [RepairDoubleToolCard Lantern Cart]

serializePathCard :: PathCard -> String
serializePathCard PathCard{ pathCardType = StartCard, north = n, east = e, south = s, west = w, rotated = r } = "s"
serializePathCard PathCard{ pathCardType = GoldCard }  = "g"
serializePathCard PathCard{ pathCardType = CoalCard }  = "c"
serializePathCard PathCard{ pathCardType = DeadEndCard }  = "0"
serializePathCard PathCard{ pathCardType = ConnectedCard }  = "1"
