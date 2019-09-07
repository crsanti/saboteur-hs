module Board where

import System.Random
import Data.List
import Card
import Utils

type Cell = (Int, Int, PathCard)
type Board = [Cell]
type Coord = (Int, Int)

goalPositions :: [Coord]
goalPositions = [
  (-2,8),
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

getCardAt :: Board -> Coord -> Maybe PathCard
getCardAt board (x,y) = do
  (_, _, c) <- find (byCoords (x, y)) board
  return c
  where
    byCoords (x,y) (x', y', _) = x == x' && y == y'

removeCardAt :: Coord -> Board -> Board
removeCardAt (x, y) = filter (not . isAtPosition)
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
  in any isGoldCardCell connections || areOtherPathsCompleted acc cell connections
  where
    areOtherPathsCompleted :: BoardAcc -> Cell -> Board -> Bool
    areOtherPathsCompleted acc cell = any $ \cell' -> isPathCompleted (nextAcc acc cell) cell'
    nextAcc :: BoardAcc -> Cell -> BoardAcc
    nextAcc (board, visited) cell = (filter (/= cell) board, cell:visited)
    isGoldCardCell :: Cell -> Bool
    isGoldCardCell (_, _, PathCard{ pathCardType = GoldCard }) = True
    isGoldCardCell _                                           = False

getCellConnections :: Cell -> Board -> Board
getCellConnections c = filter (areCellsConnected c)
  where
    areCellsConnected :: Cell -> Cell -> Bool
    areCellsConnected (x, y, p) (x', y', p') = maybe False (canFollowPath p p') $ getDirection (x, y) (x', y')

getDirection :: Coord -> Coord -> Maybe Direction
getDirection (x, y) (x', y')
  | x == x' && y + 1 == y' = return North
  | x + 1 == x' && y == y' = return East
  | x == x' && y - 1 == y' = return South
  | x - 1 == x' && y == y' = return West
  | otherwise              = fail "Not connected"

canFollowPath :: PathCard -> PathCard -> Direction -> Bool
canFollowPath PathCard{ pathCardType = DeadEndCard } _ _= False
canFollowPath _ PathCard{ pathCardType = DeadEndCard } _ = False
canFollowPath p p' d = areCardsConnected (unFlipPathCard p) d (unFlipPathCard p')
  where
    unFlipPathCard p
      | rotated p = flipPathCard p
      | otherwise = p

canPlacePathCard :: Board -> PathCard -> Coord -> Bool
canPlacePathCard b p cs | notEmpty (getCardAt b cs) = False
canPlacePathCard b p cs =
  let ps = getSurroundingCards b cs
  in canPlacePathCard' p ps || canPlacePathCard' (flipPathCard p) ps
  where
    canPlacePathCard' :: PathCard -> [(Maybe PathCard, Direction)] -> Bool
    canPlacePathCard' p = all (maybeAreCardsConnected p)
    maybeAreCardsConnected :: PathCard -> (Maybe PathCard, Direction) -> Bool
    maybeAreCardsConnected p (p', d) = maybe True (areCardsConnected p d) p'
    canCardsBePlacedSideBySide :: PathCard -> Direction -> PathCard -> Bool
    canCardsBePlacedSideBySide p North p' = north p == south p'
    canCardsBePlacedSideBySide p East  p' = east  p == west  p'
    canCardsBePlacedSideBySide p South p' = south p == north p'
    canCardsBePlacedSideBySide p West  p' = west  p == east  p'


getSurroundingCards :: Board -> Coord -> [(Maybe PathCard, Direction)]
getSurroundingCards b (x, y)= [
    (getCardAt b (x, y + 1), North),
    (getCardAt b (x + 1, y), East ),
    (getCardAt b (x, y - 1), South),
    (getCardAt b (x - 1, y), West )
  ]
