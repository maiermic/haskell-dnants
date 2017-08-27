module DNAnts.State.GameState where

import DNAnts.State.Grid (Grid)
import DNAnts.State.Population (Population)
import DNAnts.Types (AppSettings, Extents)

data GameState = GameState
  { appSettings :: AppSettings
  , nteams :: Int
  , gridExtents :: Extents
  , roundCount :: Int
  , gridFront :: Grid
  , gridBack :: Grid
  , populFront :: Population
  , populBack :: Population
  }