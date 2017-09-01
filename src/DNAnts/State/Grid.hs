module DNAnts.State.Grid where

import DNAnts.State.Cell (Cell)
import DNAnts.State.CellState (CellState)
import DNAnts.Types (Extents, Position, defaultExtents)

data Grid = Grid
  { extents :: Extents
  , cells :: [[Cell]]
  }

defaultGrid = Grid {extents = defaultExtents, cells = []}

gridWidth :: Grid -> Int
gridWidth grid = fst $ extents grid

gridHeight :: Grid -> Int
gridHeight grid = snd $ extents grid

data NeighborGrid = NeighborGrid
  { grid :: Grid
  , pos :: Position
  , teamID :: Int
  , tick :: Int
  , nilCell :: CellState
  }