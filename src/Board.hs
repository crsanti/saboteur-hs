module Board (
  Board,
  PathCard (..),
  initBoard,
  get, set,
  up, right, down, left,
  recenter, goTo,
) where

import Card
import System.Random.Shuffle (shuffleM)

data Zipper a = Zipper a Int [a] [a] deriving (Functor)

instance (Show a) => Show (Zipper a) where
  show (Zipper val pos lefts rights) =
    show (reverse (take 3 lefts)) ++ " " ++ show (val, pos) ++ " " ++ show (take 3 rights)

back, forward :: Zipper a -> Zipper a
back (Zipper val pos (l:lefts) rights) = Zipper l (pos - 1) lefts (val:rights)
forward (Zipper val pos lefts (r:rights)) = Zipper r (pos + 1) (val:lefts) rights

newtype Grid a = Grid (Zipper (Zipper a)) deriving (Functor)

instance (Show a) => Show (Grid a) where
  show (Grid (Zipper val pos lefts rights)) =
    unlines $ zipWith (\a b -> a ++ " " ++ b)
              (reverse (map show [pos - 3 .. pos + 3]))
              (map show (reverse (take 3 lefts) ++ [val] ++ take 3 rights))

up, down, left, right :: Grid a -> Grid a
up (Grid c) = Grid (forward c)
down (Grid c) = Grid (back c)
left (Grid c) = Grid (fmap back c)
right (Grid c) = Grid (fmap forward c)

get :: Grid a -> a
get (Grid (Zipper (Zipper val _ _ _) _ _ _)) = val

set :: a -> Grid a -> Grid a
set val (Grid (Zipper row pos lefts rights)) = Grid (Zipper (set' val row) pos lefts rights)
  where set' :: a -> Zipper a -> Zipper a
        set' val' (Zipper _ pos' lefts' rights') = Zipper val' pos' lefts' rights'

goTo :: Int -> Int -> Grid a -> Grid a
goTo x y g@(Grid (Zipper (Zipper _ x' _ _) y' _ _))
  | y' < y = goTo x y (up g)
  | y' > y = goTo x y (down g)
  | x' > x = goTo x y (left g)
  | x' < x = goTo x y (right g)
  | otherwise = g

recenter :: Grid a -> Grid a
recenter = goTo 0 0

type Board = Grid (Maybe PathCard)

initBoard :: Board
initBoard =
  let row  = Zipper Nothing 0 (repeat Nothing) (repeat Nothing)
      grid = Grid $ Zipper row 0 (repeat row) (repeat row)
  in setStartingPathCard (setTreasureCards grid)
  where
    -- TODO: shuffle treasures
    setTreasureCards :: Board -> Board
    setTreasureCards = let [t1, t2, t3] = treasurePool
                       in setLeftTreasureCard t1 . setCenterTreasureCard t2 . setRightTreasureCard t3
    setLeftTreasureCard, setRightTreasureCard, setCenterTreasureCard :: PathCard -> Board -> Board
    setLeftTreasureCard   t = set (Just t) . goTo 7 2
    setRightTreasureCard  t = set (Just t) . goTo 7 (-2)
    setCenterTreasureCard t = set (Just t) . goTo 7 0
    setStartingPathCard = undefined -- set (Just StartCard) . recenter
    treasurePool :: [PathCard]
    treasurePool = []

serializeBoard :: Board -> [String]
serializeBoard b = let cb = recenter b
                   in [""]
