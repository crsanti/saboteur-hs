module Board where

import System.Random
import Data.List
import Card
import Utils
import Data.Maybe

type Cell = (Int, Int, PathCard)
type MaybeCell = (Int, Int, Maybe PathCard)
type Board = [Cell]
type Coord = (Int, Int)

goalPositions :: [Coord]
goalPositions = [
  (-2,8),
  (0,8),
  (2,8)
  ]

startCardCell :: Cell
startCardCell = (0, 0, startCard)

initBoard :: StdGen -> Board
initBoard g = startCardCell : setGoalPositions (shuffleList g goalCards)
  where
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

getCellFromCoord :: Board -> Coord -> Maybe Cell
getCellFromCoord board coord@(x, y) = buildCell coord <$> getCardAt board coord
  where
    buildCell :: Coord -> PathCard -> Cell
    buildCell (x, y) c = (x, y, c)

isGoldReached :: Board -> Bool
isGoldReached b = maybe False (isPathCompletedTo b) $ find (\ (x, y, p) -> isGoldCard p) b

isPathCompletedTo :: Board -> Cell -> Bool
isPathCompletedTo b = isPathCompletedTo' b startCardCell
  where
    isPathCompletedTo' :: Board -> Cell -> Cell -> Bool
    isPathCompletedTo' board sourceCell destinationCell =
      let connections = getCellConnections sourceCell board
      in destinationCell `elem` connections ||
        any (\nextSourceCell -> isPathCompletedTo' (filter (/= sourceCell) board) nextSourceCell destinationCell) connections

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
      | rotated p = flipPathCard p
      | otherwise = p

canPlacePathCard :: Board -> PathCard -> Coord -> Bool
canPlacePathCard b p cs | notEmpty (getCardAt b cs) = False
canPlacePathCard b p cs =
  let ps = getSurroundingCardsWithDirection b cs
  in canPlacePathCard' p ps || canPlacePathCard' (flipPathCard p) ps
  where
    canPlacePathCard' :: PathCard -> [(PathCard, Direction)] -> Bool
    canPlacePathCard' p = all (\(p', d) -> areCardsConnected p d p')
    canCardsBePlacedSideBySide :: PathCard -> Direction -> PathCard -> Bool
    canCardsBePlacedSideBySide p North p' = north p == south p'
    canCardsBePlacedSideBySide p East  p' = east  p == west  p'
    canCardsBePlacedSideBySide p South p' = south p == north p'
    canCardsBePlacedSideBySide p West  p' = west  p == east  p'

getSurroundingCells :: Board -> Coord -> [MaybeCell]
getSurroundingCells b (x, y) = [
  getSurroundingCell b (x, y + 1),
  getSurroundingCell b (x + 1, y),
  getSurroundingCell b (x, y - 1),
  getSurroundingCell b (x - 1, y)
  ]
  where
    getSurroundingCell :: Board -> Coord -> MaybeCell
    getSurroundingCell b c@(x, y) = (x, y, getCardAt b c)

getSurroundingCellsWithDirection :: Board -> Coord -> [(MaybeCell, Direction)]
getSurroundingCellsWithDirection b cs = zip (getSurroundingCells b cs) (enumFrom North)

getSurroundingCardsWithDirection :: Board -> Coord -> [(PathCard, Direction)]
getSurroundingCardsWithDirection b cs = [(x, d) | ((_, _, Just x), d) <- getSurroundingCellsWithDirection b cs]

getSurroundingCards :: Board -> Coord -> [PathCard]
getSurroundingCards b cs = [x | ((_, _, Just x), _) <- getSurroundingCellsWithDirection b cs]

getSurroundingEmptyPlaces :: Board -> Coord -> [Coord]
getSurroundingEmptyPlaces b cs = [(x, y) | (x, y, Nothing) <- getSurroundingCells b cs]

getEmptyCellsFromStart :: Board -> [Coord]
getEmptyCellsFromStart b = nub $ getEmptyPlaces' (b, []) startCardCell
  where
    getEmptyPlaces' :: (Board, Board) -> Cell -> [Coord]
    getEmptyPlaces' (board, visited) cell@(x, y, card) =
      let connections  = [(x', y', p) | (x', y', Just p) <- getSurroundingCells board (x, y), (x', y', p) `notElem` visited]
          emptyPlaces' = getSurroundingEmptyPlaces board (x, y)
      in emptyPlaces' ++ concatMap (getEmptyPlaces' (board, cell:visited)) connections

getUnflippedGoalCells :: Board -> [Cell]
getUnflippedGoalCells b = filter (not . isPathCompletedTo b) $ filter isCellGoalCard b
  where
    isCellGoalCard :: Cell -> Bool
    isCellGoalCard (_, _, c) = isGoalCard c