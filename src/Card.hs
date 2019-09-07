module Card where

import System.Random
import Utils

data GoldNuggetCard = GoldNugget1
                    | GoldNugget2
                    | GoldNugget3
                    deriving (Eq)

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
  show (BrokenToolCard t)           = "(💕" ++ show t ++ ")"
  show (RepairToolCard t)           = "(💕" ++ show t ++ ")"
  show (RepairDoubleToolCard t1 t2) = "(💕" ++ show t1 ++ show t2 ++ ")"
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

instance Show PathCard where
  show PathCard{ pathCardType = StoneCard,     north = True,  east = True,  south = False, west = False, rotated = False } = "🌑 └"
  show PathCard{ pathCardType = StoneCard,     north = True,  east = True,  south = False, west = False, rotated = True  } = "🌑 ┐"
  show PathCard{ pathCardType = GoldCard,      north = True,  east = True,  south = True,  west = True,  rotated = _     } = "🏆 ┼"
  show PathCard{ pathCardType = StartCard,     north = True,  east = True,  south = True,  west = True,  rotated = _     } = "🏠 ┼"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True,  rotated = _     } = "✅ ┼"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  rotated = False } = "✅ ┴"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  rotated = True  } = "✅ ┬"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = False, rotated = False } = "✅ ├"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = False, rotated = True  } = "✅ ┤"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  rotated = False } = "✅ ┘"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  rotated = True  } = "✅ ┌"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = False } = "✅ └"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = True  } = "✅ ┐"
  show PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, rotated = _     } = "✅ │"
  show PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True,  rotated = _     } = "✅ ─"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = True,  rotated = _     } = "❌ ┼"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = True,  rotated = False } = "❌ ┴"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = True,  rotated = True  } = "❌ ┬"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = False, rotated = False } = "❌ ├"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = False, rotated = True  } = "❌ ┤"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = True,  rotated = False } = "❌ ┘"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = True,  rotated = True  } = "❌ ┌"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = False, rotated = False } = "❌ └"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = False, rotated = True  } = "❌ ┐"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = True,  west = False, rotated = _     } = "❌ │"
  show PathCard{ pathCardType = DeadEndCard,   north = False, east = True,  south = False, west = True,  rotated = _     } = "❌ ─"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = False, rotated = False } = "❌ ╷"
  show PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = False, rotated = True  } = "❌ ╵"
  show PathCard{ pathCardType = DeadEndCard,   north = False,  east = False, south = False, west = True, rotated = False } = "❌ ╴"
  show PathCard{ pathCardType = DeadEndCard,   north = False,  east = False, south = False, west = True, rotated = True  } = "❌ ╶"

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
areCardsConnected :: PathCard -> Direction -> PathCard -> Bool
areCardsConnected p North p' = north p && south p'
areCardsConnected p East  p' = east  p && west  p'
areCardsConnected p South p' = south p && north p'
areCardsConnected p West  p' = west  p && east  p'

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

stringToCard :: String -> PathCard
stringToCard "R11000" = PathCard{ pathCardType = StoneCard,     north = True,  east = True,  south = False, west = False, rotated = False }
stringToCard "R11001" = PathCard{ pathCardType = StoneCard,     north = True,  east = True,  south = False, west = False, rotated = True  }
stringToCard "G11110" = PathCard{ pathCardType = GoldCard,      north = True,  east = True,  south = True,  west = True,  rotated = False }
stringToCard "G11111" = PathCard{ pathCardType = GoldCard,      north = True,  east = True,  south = True,  west = True,  rotated = True  }
stringToCard "S11110" = PathCard{ pathCardType = StartCard,     north = True,  east = True,  south = True,  west = True,  rotated = False }
stringToCard "S11111" = PathCard{ pathCardType = StartCard,     north = True,  east = True,  south = True,  west = True,  rotated = True  }
stringToCard "C11110" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True,  rotated = False }
stringToCard "C11111" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = True,  rotated = True  }
stringToCard "C11010" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  rotated = False }
stringToCard "C11011" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = True,  rotated = True  }
stringToCard "C11100" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = False, rotated = False }
stringToCard "C11101" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = True,  west = False, rotated = True  }
stringToCard "C10010" = PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  rotated = False }
stringToCard "C10011" = PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = False, west = True,  rotated = True  }
stringToCard "C11000" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = False }
stringToCard "C11001" = PathCard{ pathCardType = ConnectedCard, north = True,  east = True,  south = False, west = False, rotated = True  }
stringToCard "C10100" = PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, rotated = False }
stringToCard "C10101" = PathCard{ pathCardType = ConnectedCard, north = True,  east = False, south = True,  west = False, rotated = True  }
stringToCard "C01010" = PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True,  rotated = False }
stringToCard "C01011" = PathCard{ pathCardType = ConnectedCard, north = False, east = True,  south = False, west = True,  rotated = True  }
stringToCard "X11110" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = True,  rotated = False }
stringToCard "X11111" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = True,  rotated = True  }
stringToCard "X11010" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = True,  rotated = False }
stringToCard "X11011" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = True,  rotated = True  }
stringToCard "X11100" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = False, rotated = False }
stringToCard "X11101" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = True,  west = False, rotated = True  }
stringToCard "X10010" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = True,  rotated = False }
stringToCard "X10011" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = True,  rotated = True  }
stringToCard "X11000" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = False, rotated = False }
stringToCard "X11001" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = True,  south = False, west = False, rotated = True  }
stringToCard "X10100" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = True,  west = False, rotated = False }
stringToCard "X10101" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = True,  west = False, rotated = True  }
stringToCard "X01010" = PathCard{ pathCardType = DeadEndCard,   north = False, east = True,  south = False, west = True,  rotated = False }
stringToCard "X01011" = PathCard{ pathCardType = DeadEndCard,   north = False, east = True,  south = False, west = True,  rotated = True  }
stringToCard "X10000" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = False, rotated = False }
stringToCard "X10001" = PathCard{ pathCardType = DeadEndCard,   north = True,  east = False, south = False, west = False, rotated = True  }
stringToCard "X00010" = PathCard{ pathCardType = DeadEndCard,   north = False, east = False, south = False, west = True,  rotated = False }
stringToCard "X00011" = PathCard{ pathCardType = DeadEndCard,   north = False, east = False, south = False, west = True,  rotated = True  }
stringToCard s = error $ "Cannot convert card '" ++ s ++ "'"

startCard :: PathCard
startCard = PathCard{ pathCardType = StartCard, north = True, east = True, south = True, west = True, rotated = False }

goldCard :: PathCard
goldCard = PathCard{ pathCardType = GoldCard, north = True, east = True, south = True, west = True, rotated = False }

stoneCard :: PathCard
stoneCard = PathCard{ pathCardType = StoneCard, north = True, east = True, south = False, west = False, rotated = False }
