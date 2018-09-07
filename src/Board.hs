module Board (
) where

import Card

type Cell = (Int, Int, Maybe PathCard)
type Board = [Cell]
