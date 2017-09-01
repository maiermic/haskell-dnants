module DNAnts.State.Grid where

import DNAnts.State.Cell (Cell)
import DNAnts.State.CellState (CellState)
import DNAnts.Types (Extents, Position, defaultExtents)

data Grid = Grid
  { extents :: Extents
  , cells :: [[Cell]]
  }

defaultGrid :: Grid
defaultGrid = Grid {extents = defaultExtents, cells = []}

gridWidth :: Grid -> Int
gridWidth grid = fst $ extents grid

gridHeight :: Grid -> Int
gridHeight grid = snd $ extents grid

indexedCells :: Grid -> [(Cell, (Int, Int))]
indexedCells = concat . addIndex2 . cells

addIndex :: [a] -> [(a, Int)]
addIndex a = zip a [0 ..]

addIndex2 :: [[a]] -> [[(a, (Int, Int))]]
addIndex2 a =
  map (\(row, y) -> map (\(value, x) -> (value, (x, y))) $ addIndex row) $
  addIndex a


data NeighborGrid = NeighborGrid
  { grid :: Grid
  , pos :: Position
  , teamID :: Int
  , tick :: Int
  , nilCell :: CellState
  }