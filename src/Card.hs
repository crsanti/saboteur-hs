module Card where

import System.Random
import Utils

data GoldNuggetCard = GoldNugget1
                    | GoldNugget2
                    | GoldNugget3

instance Show GoldNuggetCard where
  show GoldNugget1 = "(💎)"
  show GoldNugget2 = "(💎💎)"
  show GoldNugget3 = "(💎💎💎)"

data Tool = Pickaxe
          | Lantern
          | Cart
          deriving (Eq)

instance Show Tool where
  show Pickaxe = "⛏ "
  show Lantern = "🕯 "
  show Cart    = "🚐"

data ActionCard = BrokenToolCard Tool
                | RepairToolCard Tool
                | RepairDoubleToolCard Tool Tool
                | RockFallCard
                | MapCard
                deriving (Eq)

instance Show ActionCard where
  show (BrokenToolCard t)           = "(💕" ++ (show t) ++ ")"
  show (RepairToolCard t)           = "(💕" ++ (show t) ++ ")"
  show (RepairDoubleToolCard t1 t2) = "(💕" ++ (show t1) ++ (show t2) ++ ")"
  show RockFallCard                 = "☄️ "
  show MapCard                      = "🗺 "

data PathCardType = ConnectedCard
                  | DeadEndCard
                  | StartCard
                  | GoldCard
                  | StoneCard
                  deriving (Show, Eq)

data PathCard = PathCard{
  pathCardType :: PathCardType,
  north :: Bool,
  east :: Bool,
  south :: Bool,
  west :: Bool,
  rotated :: Bool
} deriving (Eq)

serializeSides :: PathCard -> String
serializeSides PathCard{ north = n, east = e, south = s, west = w, rotated = r } =
  [(boolToChar n), (boolToChar e), (boolToChar s), (boolToChar w), (boolToChar r)]

instance Show PathCard where
  show p@PathCard{ pathCardType = ConnectedCard } = "🔌 " ++ (serializeSides p)
  show p@PathCard{ pathCardType = DeadEndCard   } = "❌ " ++ (serializeSides p)
  show p@PathCard{ pathCardType = StartCard     } = "🏠 " ++ (serializeSides p)
  show p@PathCard{ pathCardType = GoldCard      } = "🏆 " ++ (serializeSides p)
  show p@PathCard{ pathCardType = StoneCard     } = "🌑 " ++ (serializeSides p)

type CardProb = Int
data Deck = Deck [PathCard] [ActionCard]
            deriving (Show, Eq)

flipPathCard :: PathCard -> PathCard
flipPathCard PathCard{ pathCardType = p, north = n, east = e, south = s, west = w, rotated = r } =
  PathCard{ pathCardType = p, north = s, east = w, south = n, west = e, rotated = not r }

data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq)

-- | areCardsConnected :: base PathCard -> PathCard to check -> Direction to check
areCardsConnected :: PathCard -> PathCard -> Direction -> Bool
areCardsConnected p p' d | rotated p  = areCardsConnected (flipPathCard p) p'                d
areCardsConnected p p' d | rotated p' = areCardsConnected p                (flipPathCard p') d
areCardsConnected p p' North          = north p && south p'
areCardsConnected p p' East           = east  p && west  p'
areCardsConnected p p' South          = south p && north p'
areCardsConnected p p' West           = west  p && east  p'

getPathCardTypeFromDirection :: Direction -> PathCard -> Bool
getPathCardTypeFromDirection North p = south p
getPathCardTypeFromDirection East  p = west  p
getPathCardTypeFromDirection South p = north p
getPathCardTypeFromDirection West  p = east  p

newDeck :: Deck
newDeck = Deck initPathCards initActionCards
  where
    initPathCards :: [PathCard]
    initPathCards =
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True, rotated = False })  ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = False }) ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True, rotated = False })  ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = True, rotated = False })  ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True, rotated = False })  ++
      replicate 5 (PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True, rotated = False })  ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = False, west = True, rotated = False }]  ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = False, south = True,  west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = False, east = True,  south = False, west = True, rotated = False }]  ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = False, rotated = False }] ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = False, west = True, rotated = False }]  ++
      [PathCard{ pathCardType = DeadEndCard, north = True,  east = True,  south = True,  west = True, rotated = False }]
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

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen (Deck ps as) = Deck (shuffleList gen ps) (shuffleList gen as)

newGoldenDeck :: [GoldNuggetCard]
newGoldenDeck = replicate 16 GoldNugget1 ++ replicate 8 GoldNugget2 ++ replicate 4 GoldNugget3

pathCardTypeToChar :: PathCardType -> Char
pathCardTypeToChar ConnectedCard = 'C'
pathCardTypeToChar DeadEndCard   = 'X'
pathCardTypeToChar StartCard     = 'S'
pathCardTypeToChar GoldCard      = 'G'
pathCardTypeToChar StoneCard     = 'R'

charToPathCardType :: Char -> PathCardType
charToPathCardType 'C' = ConnectedCard
charToPathCardType 'X' = DeadEndCard
charToPathCardType 'S' = StartCard
charToPathCardType 'G' = GoldCard
charToPathCardType 'R' = StoneCard

cardToString :: PathCard -> String
cardToString p@PathCard{ pathCardType = t } = pathCardTypeToChar t : serializeSides p

stringToCard :: String -> PathCard
stringToCard [t, n, e, s, w, r] = PathCard{
  pathCardType = charToPathCardType t,
  north = charToBool n,
  east = charToBool e,
  south = charToBool s,
  west = charToBool w,
  rotated = charToBool r
}

startCard :: PathCard
startCard = PathCard{ pathCardType = StartCard, north = True, east = True, south = True, west = True, rotated = False }

goldCard :: PathCard
goldCard = PathCard{ pathCardType = GoldCard, north = True, east = True, south = True, west = True, rotated = False }

stoneCard :: PathCard
stoneCard = PathCard{ pathCardType = StoneCard, north = True, east = True, south = False, west = False, rotated = False }
