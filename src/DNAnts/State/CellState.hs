{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  deriving (Eq, Show)

data CellState = CellState
  { _cellType :: CellType
  , _amount :: Int
  , _taken :: Bool
  , _antID :: Maybe AntId
  , _tracesIn :: [Traces]
  , _tracesOut :: [Traces]
  }

instance Show CellState where
  show CellState {_cellType, _amount, _taken, _antID} =
    "CellState { type: " ++
    show _cellType ++
    ", amount: " ++
    show _amount ++
    ", taken: " ++ show _taken ++ ", antID: " ++ show _antID ++ " }"

makeLenses ''CellState

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