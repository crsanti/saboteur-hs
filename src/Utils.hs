module Utils where

import Data.List
import Data.Bool
import System.Random.Shuffle
import System.Random

randomList :: Int -> StdGen -> ([Int], StdGen)
randomList 0 g   = ([], g)
randomList n gen =
  let (x, gen')   = randomR (1, 10) gen
      (xs, gen'') = randomList (n - 1) gen'
  in (x:xs, gen'')

shuffleList :: StdGen -> [a] -> [a]
shuffleList g xs = shuffle' xs (length xs) g

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

getRandomProbs :: Int -> StdGen -> ([Int], StdGen)
getRandomProbs x s =
  let nCards = getNumberOfHandCards x
  in randomList nCards s
  where
    getNumberOfHandCards :: Int -> Int
    getNumberOfHandCards x
      | x >= 3 && x <= 5 = 6
      | x == 6 || x == 7 = 5
      | x > 7 || x <= 10 = 4

nextElem :: Eq a => a -> [a] -> Maybe a
nextElem x xs@(_:_) = lookup x $ (zip <*> tail) (cycle xs)
nextElem _ []       = Nothing

boolToChar :: Bool -> Char
boolToChar = bool '0' '1'

charToBool :: Char -> Bool
charToBool '0' = False
charToBool '1' = True
