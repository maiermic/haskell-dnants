{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.CellState where

import Control.Lens
import DNAnts.State.AntId (AntId)

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
  deriving Eq

data CellState = CellState
  { _cellType :: CellType
  , _amount :: Int
  , _taken :: Bool
  , _antID :: Maybe AntId
  , _tracesIn :: [Traces]
  , _tracesOut :: [Traces]
  }

makeLenses '' CellState

defaultCellState =
  CellState
  { _cellType = Plain
  , _amount = 0
  , _taken = False
  , _antID = Nothing
  , _tracesIn = undefined
  , _tracesOut = undefined
  }

numFood :: Lens' CellState Int
numFood = amount