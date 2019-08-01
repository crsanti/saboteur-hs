module Board where

import System.Random
import Data.List
import Card
import Utils
import Data.Maybe

-- | Cell :: (x, y, PathCard)
type Cell = (Int, Int, PathCard)
type Board = [Cell]
type Coord = (Int, Int)

goalPositions :: [Coord]
goalPositions = [
  ((-2),8),
  (0,8),
  (2,8)
  ]

initBoard :: StdGen -> Board
initBoard g = startCardWithPosition : setGoalPositions (shuffleList g goalCards)
  where
    startCardWithPosition = (0, 0, startCard)
    goalCards = [goldCard, stoneCard, stoneCard]
    setGoalPositions :: [PathCard] -> [Cell]
    setGoalPositions = zipWith (\(x,y) c -> (x,y,c)) goalPositions

getCardAt :: Int -> Int -> Board -> Maybe PathCard
getCardAt x y board = do
  (_, _, card@PathCard{ pathCardType = t }) <- find (byCoords x y) board
  case t of
    ConnectedCard -> return card
    DeadEndCard   -> return card
    _             -> fail "Not a valid PathCard"
  where
    byCoords x y (x', y', _) = x == x' && y == y'

removeCardAt :: Coord -> Board -> Board
removeCardAt (x, y) board = filter (not . isAtPosition) board
  where
    isAtPosition :: Cell -> Bool
    isAtPosition (x', y', _) = x /= x' && y /= y'


isGoalReached :: Board -> Bool
isGoalReached [] = False
isGoalReached b = isPathCompleted (b, []) (0, 0, startCard)
  where
    boardWithoutStartCard = removeCardAt (0, 0) b

type BoardAcc = (Board, Board)
isPathCompleted :: (Board, Board) -> Cell -> Bool
isPathCompleted acc cell =
  let connections = getCellConnections cell (fst acc)
  in
    any isGoldCardCell connections                 -- One connection is gold
    || areOtherPathsCompleted acc cell connections -- Check other connections
  where
    hasConnections :: Board -> Bool
    hasConnections = not . null
    areOtherPathsCompleted :: BoardAcc -> Cell -> Board -> Bool
    areOtherPathsCompleted acc cell = any $ \cell' -> isPathCompleted (nextAcc acc cell) cell'
    nextAcc :: BoardAcc -> Cell -> BoardAcc
    nextAcc (board, visited) cell = (filter (not . (== cell)) board, cell:visited)
    isGoldCardCell :: Cell -> Bool
    isGoldCardCell (_, _, PathCard{ pathCardType = GoldCard }) = True
    isGoldCardCell _ = False

-- 1) The card has no connections? False
-- 2) The card has connection but is not the gold card? Watch all connections
-- 3) The card is connected to gold card? True

getCellConnections :: Cell -> Board -> Board
getCellConnections c = filter (areCellsConnected c)
  where
    areCellsConnected :: Cell -> Cell -> Bool
    areCellsConnected (x, y, p) (x', y', p') = maybe False (canFollowPath p p') $ getDirection (x, y) (x', y')

getDirection :: Coord -> Coord -> Maybe Direction
getDirection (x, y) (x', y')
  | (x == x' && y + 1 == y') = return North
  | (x + 1 == x' && y == y') = return East
  | (x == x' && y - 1 == y') = return South
  | (x - 1 == x' && y == y') = return West
  | otherwise                = fail "Not connected"

canFollowPath :: PathCard -> PathCard -> Direction -> Bool
canFollowPath p p' _ |
  pathCardType p  == DeadEndCard ||
  pathCardType p' == DeadEndCard = False
canFollowPath p p' d               = areCardsConnected p p' d
