{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.AntState where

import DNAnts.Types (Direction, Position)

data AntId = AntId
  { teamId :: Int
  , id :: Int
  }

-- TODO add comments
data AntMode
  = Waiting
  | Scouting
  | Haming
  | Eating
  | Harvesting
  | Dead

data AntAction
  = DoIdle
  | DoMove
  | DoEat
  | DoHarvest
  | DoDrop
  | DoAttack
  | DoTurn

data StateEvents = StateEvents
  { collision :: Bool
  , attacked :: Bool
  , food :: Bool
  , enemy :: Bool
  }

defaultStateEvents =
  StateEvents {collision = False, attacked = False, food = False, enemy = False}

data AntState = AntState
  { id :: Int
  , teamID :: Int
  , pos :: Position
  , dist :: Position
  , dir :: Direction
  , rand :: Int
  , lastDirChange :: Int
  , strength :: Int
  , damage :: Int
  , numCarrying :: Int
  , nticksNotFed :: Int
  , tickCount :: Int
  , events :: StateEvents
  , enemyDir :: Direction
  , action :: AntAction
  , mode :: AntMode
  }

defaultAntState =
  AntState
  { id = 0
  , teamID = 0
  , pos = (0, 0)
  , dist = (0, 0)
  , dir = (0, 0)
  , rand = 0
  , lastDirChange = 0
  , strength = 0
  , damage = 0
  , numCarrying = 0
  , nticksNotFed = 0
  , tickCount = 0
  , events = defaultStateEvents
  , enemyDir = (0, 0)
  , action = DoMove
  , mode = Scouting
  }