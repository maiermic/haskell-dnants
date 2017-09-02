{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.Map where

import Control.Lens.Operators
-- TODO explizi imports
import Control.Lens.Traversal
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import DNAnts.State.Cell (Cell(Cell), defaultCell)
import DNAnts.State.CellState
       (CellType(Barrier, Food), cellType, defaultCellState)
import DNAnts.State.Grid
       (Grid(Grid, _cells, _extents), defaultGrid)
import qualified DNAnts.State.Grid as G
import DNAnts.State.Population (Population)
import DNAnts.Types (Extents, defaultExtents)

data MapConfig = MapConfig
  { extents :: Extents
  , numGrassRegions :: Int
  , numFoodRegions :: Int
  , numBarriers :: Int
  , numTeams :: Int
  , teamSize :: Int
  , symmetric :: Bool
  }

defaultMapConfig =
  MapConfig
  { extents = defaultExtents
  , numGrassRegions = 0
  , numFoodRegions = 0
  , numBarriers = 0
  , numTeams = 0
  , teamSize = 0
  , symmetric = False
  }

data Map = Map
  { grid :: Grid
  , population :: Population
  }

generateMap :: MapConfig -> IO Map
generateMap config@MapConfig {extents} = do
  grid <- generateGrid extents
  return $ Map {grid, population = undefined}

--  population <- generatePopulation
initialGrid :: Extents -> Cell -> GridCells
initialGrid (gridWidth, gridHeight) cell =
  replicate gridHeight $ replicate gridWidth cell

type GridCells = [[Cell]]

generateGrid :: Extents -> IO Grid
generateGrid _extents = do
  _cells <-
    execStateT (generateGridCells _extents) $ initialGrid _extents $
    Cell defaultCellState {cellType = Barrier}
  return Grid {_cells, _extents}

generateGridCells :: Extents -> StateT GridCells IO ()
generateGridCells extends = do
  traverse . traverse .= Cell defaultCellState {cellType = Food}

generatePopulation :: StateT Population IO ()
generatePopulation = undefined