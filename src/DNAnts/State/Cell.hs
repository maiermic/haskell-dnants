{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.Cell where

import Control.Lens
import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.Lens
import DNAnts.State.CellState
import Lens.Family2.State.Lazy (zoom)

data Cell =
  Cell CellState

defaultCell = Cell defaultCellState

updateCell :: StateT Cell IO ()
updateCell = return () -- TODO lower intensity of traces

cellState :: Cell -> CellState
cellState (Cell _cellState) = _cellState

cellStateL :: Optic' (->) (Const Bool) Cell CellState
cellStateL = to cellState

cellTypeL :: Optic' (->) (Const Bool) Cell CellType
cellTypeL = cellStateL . cellType

isCellTypeL :: CellType -> Optic' (->) (Const Bool) Cell Bool
isCellTypeL t = cellTypeL . to (== t)

isCellTaken :: Cell -> Bool
isCellTaken = view $ cellStateL . taken

isObstacle :: Cell -> Bool
isObstacle = view $ isCellTypeL Barrier

isNotSpawnPoint :: Optic' (->) (Const Bool) Cell Bool
isNotSpawnPoint = cellTypeL ./= SpawnPoint

containsFood :: Optic' (->) (Const Bool) Cell Bool
containsFood = gtL (cellStateL . numFood) 0