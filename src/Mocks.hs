module Mocks where

import System.Random
-- import Control.Monad.State

-- import State
import Card
import Player
import Game
import Board

newGen :: StdGen
newGen = mkStdGen 0

mockGame :: Game
mockGame = initGame ["p1", "p2", "p3"] newGen


listToBoard :: [(Int, Int, String)] -> Board
listToBoard = map (\(x,y,c) -> (x,y, stringToCard c))

finishedBoard1 :: Board
finishedBoard1 = listToBoard [
  (-2,8,"R11000"), (0,8,"R11000"),                 (2,8,"G11110"),
                   (0,7,"C10011"), (1,7,"C01010"), (2,7,"C10010"),
                   (0,6,"C10100"),
                   (0,5,"C10100"),
                   (0,4,"C10100"),
                   (0,3,"C10100"),
                   (0,2,"C10100"),
                   (0,1,"C10100"),
                   (0,0,"S11110")
  ]

-- https://i0.wp.com/www.teamboardgame.com/wp-content/uploads/2015/04/Saboteur-Overview.jpg
finishedBoard2 :: Board
finishedBoard2 = listToBoard [
  (-2, 8, "R11000"), (-1, 8, "X10100"), (0, 8, "G11110"),                   (2, 8, "R11001"),
  (-2, 7, "X11000"), (-1, 7, "C11010"), (0, 7, "C11110"), (1, 7, "C01010"), (2, 7, "X11110"),
  (-2, 6, "C10011"), (-1, 6, "C01010"), (0, 6, "C11110"), (1, 6, "X00010"), (2, 6, "C10100"),
  (-2, 5, "C11000"), (-1, 5, "C11011"), (0, 5, "C11110"), (1, 5, "C01010"), (2, 5, "C11101"),
  (-2, 4, "C10011"), (-1, 4, "C11110"), (0, 4, "C10010"), (1, 4, "C10011"), (2, 4, "C11101"),
  (-2, 3, "C10100"), (-1, 3, "C11000"), (0, 3, "X11011"), (1, 3, "C11110"), (2, 3, "X11101"),
  (-2, 2, "C11100"), (-1, 2, "C01010"), (0, 2, "C11010"), (1, 2, "C11010"), (2, 2, "C11101"),
  (-2, 1, "C11100"), (-1, 1, "C11001"), (0, 1, "C10011"), (1, 1, "C11001"), (2, 1, "C10100"),
  (-2, 0, "X10000"), (-1, 0, "C11000"), (0, 0, "S11110"), (1, 0, "C11010"), (2, 0, "C10010")
  ]
