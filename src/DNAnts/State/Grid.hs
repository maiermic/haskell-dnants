{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.Grid where

import Control.Lens (makeLenses, view)
import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.State.Cell
import DNAnts.State.CellState (CellState)
import DNAnts.Types (Extents, Position, defaultExtents)
import Lens.Family2.State.Lazy (zoom)
import SDL.Vect

data Grid = Grid
  { _extents :: Extents
  , _cells :: [[Cell]]
  }

makeLenses ''Grid

defaultGrid :: Grid
defaultGrid = Grid {_extents = defaultExtents, _cells = []}

gridWidth :: Grid -> Int
gridWidth grid = view _x $ _extents grid

gridHeight :: Grid -> Int
gridHeight grid = view _y $ _extents grid

indexedCells :: Grid -> [(Cell, Position)]
indexedCells = concat . addIndex2 . _cells

addIndex :: [a] -> [(a, Int)]
addIndex a = zip a [0 ..]

addIndex2 :: [[a]] -> [[(a, Position)]]
addIndex2 a =
  map (\(row, y) -> map (\(value, x) -> (value, V2 x y)) $ addIndex row) $
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

{- |
Access cell at a specific point in the grid.

>>> cellAt (V2 2 1) [[0..2],[3..5]]
5

-}
cellAt :: Position -> [[Cell]] -> Cell
cellAt pos@(V2 x y) = (!! x) . (!! y)

containsPosition :: Position -> Grid -> Bool
containsPosition (V2 x y) Grid {_extents = V2 w h} =
  and [x >= 0, y >= 0, x < w, y < h]

allowsMoveTo :: Position -> Grid -> Bool
allowsMoveTo pos grid =
  let cell = cellAt pos $ _cells grid
  in and
       [ containsPosition pos grid
       , not $ isCellTaken cell
       , not $ isObstacle cell
       ]