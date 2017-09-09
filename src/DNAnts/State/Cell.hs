{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.Cell where

import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.State.CellState
import Control.Lens
import Lens.Family2.State.Lazy (zoom)

data Cell =
  Cell CellState

defaultCell = Cell defaultCellState

updateCell :: StateT Cell IO ()
updateCell = return () -- TODO lower intensity of traces

cellState (Cell _cellState) = _cellState

cellStateL = to cellState

cellTypeL = cellStateL . cellType

isCellTypeL t = cellTypeL . to (== t)

isCellTaken :: Cell -> Bool
isCellTaken = view $ cellStateL . taken

isObstacle :: Cell -> Bool
isObstacle = view $ isCellTypeL Barrier