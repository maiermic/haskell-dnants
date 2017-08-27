module DNAnts.State.CellState where

import DNAnts.State.AntState (AntId)

type Trace = Int

type Traces = [Trace]

data CellType
  = Plain
  | None
  | Barrier
  | SpawnPoint
  | Grass
  | Water
  | Food

data CellState = CellState
  { cellType :: CellType
  , amount :: Int
  , taken :: Bool
  , antID :: AntId
  , tracesIn :: [Traces]
  , tracesOut :: [Traces]
  }

defaultCellState =
  CellState
  { cellType = Plain
  , amount = 0
  , taken = False
  , antID = undefined
  , tracesIn = undefined
  , tracesOut = undefined
  }