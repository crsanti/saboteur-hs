module PrettyPrint where

import Data.List
import Data.Function
import Data.Maybe

import Board
import Card
import Utils

prettyPrintBoard :: Board -> String
prettyPrintBoard b =
  let maxX   = maxByX b
      maxY   = maxByY b
      minX   = minByX b
      minY   = minByY b
      matrix = [maybe " " showWithoutIndicator (getCardAt b (x, y)) | y <- range maxY minY, x <- range minX maxX]
    in concat $ intercalate ["\n"] $ splitEvery (maxX - minX + 1) matrix
  where
    getX :: Cell -> Int
    getX = fst . tripleToPair
    getY :: Cell -> Int
    getY = snd . tripleToPair
    maxByX :: Board -> Int
    maxByX = maximum . map getX
    maxByY :: Board -> Int
    maxByY = maximum . map getY
    minByX :: Board -> Int
    minByX = minimum . map getX
    minByY :: Board -> Int
    minByY = minimum . map getY

range :: Int -> Int -> [Int]
range a b | a == b    = []
          | otherwise = [a, a + signum (b - a)..b]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list


showWithoutIndicator :: PathCard -> String
showWithoutIndicator p = (:[]) $ last $ show p
