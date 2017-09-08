{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.Grid where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.State.Cell (Cell, updateCell)
import DNAnts.State.CellState (CellState)
import DNAnts.Types (Extents, Position, defaultExtents)
import Lens.Family2.State.Lazy (zoom)

data Grid = Grid
  { _extents :: Extents
  , _cells :: [[Cell]]
  }

makeLenses ''Grid

defaultGrid :: Grid
defaultGrid = Grid {_extents = defaultExtents, _cells = []}

gridWidth :: Grid -> Int
gridWidth grid = fst $ _extents grid

gridHeight :: Grid -> Int
gridHeight grid = snd $ _extents grid

indexedCells :: Grid -> [(Cell, (Int, Int))]
indexedCells = concat . addIndex2 . _cells

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

updateGrid :: StateT Grid IO ()
updateGrid = zoom (cells . traverse . traverse) updateCell