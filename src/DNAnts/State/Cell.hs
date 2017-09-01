module DNAnts.State.Cell where

import DNAnts.State.CellState (CellState(CellState), defaultCellState)

data Cell = Cell CellState

defaultCell = Cell defaultCellState