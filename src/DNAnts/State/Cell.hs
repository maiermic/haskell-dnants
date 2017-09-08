{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.Cell where

import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.State.CellState
import Lens.Family2.State.Lazy (zoom)

data Cell =
  Cell CellState

defaultCell = Cell defaultCellState

updateCell :: StateT Cell IO ()
updateCell = return () -- TODO lower intensity of traces

isCellTaken :: Cell -> Bool
isCellTaken (Cell CellState {taken}) = taken

isObstacle :: Cell -> Bool
isObstacle (Cell CellState {cellType}) = cellType == Barrier