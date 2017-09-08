module DNAnts.State.Cell where

import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.State.CellState
       (CellState(CellState), defaultCellState)
import Lens.Family2.State.Lazy (zoom)

data Cell =
  Cell CellState

defaultCell = Cell defaultCellState

updateCell :: StateT Cell IO ()
updateCell = return () -- TODO lower intensity of traces