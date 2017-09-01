module DNAnts.State.Map
  ( MapConfig(..)
  , defaultMapConfig
  ) where

module DNAnts.State.Map where

import DNAnts.State.Grid (Grid(_extents), defaultGrid)
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

generateMap MapConfig {} = undefined