module DNAnts.State.Grid
  ( Grid(..)
  , defaultGrid
  ) where

import DNAnts.State.Cell (Cell)
import DNAnts.State.CellState (CellState)
import DNAnts.Types (Extents, Position, defaultExtents)

data Grid = Grid
  { extents :: Extents
  , cells :: [[Cell]]
  }

defaultGrid = Grid {extents = defaultExtents, cells = []}

data NeighborGrid = NeighborGrid
  { grid :: Grid
  , pos :: Position
  , teamID :: Int
  , tick :: Int
  , nilCell :: CellState
  }