{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.AntState where

import Control.Lens
import Control.Monad.Trans.State.Lazy (StateT)
import DNAnts.Lens
import DNAnts.Types (Direction, Position)
import SDL.Vect

-- TODO add comments
data AntMode
  = Waiting
  | Scouting
  | Haming
  | Eating
  | Harvesting
  | Dead
  deriving (Eq, Show)

data AntAction
  = DoIdle
  | DoMove
  | DoEat
  | DoHarvest
  | DoDrop
  | DoAttack
  | DoTurn
  deriving (Eq, Show)

data StateEvents = StateEvents
  { _collision :: Bool
  , _attacked :: Bool
  , _food :: Bool
  , _enemy :: Bool
  } deriving (Eq)

defaultStateEvents =
  StateEvents
  {_collision = False, _attacked = False, _food = False, _enemy = False}

data AntState = AntState
  { id :: Int
  , teamID :: Int
  , _pos :: V2 Int
  , _dist :: Position
  , _dir :: V2 Int
  , lastDirChange :: Int
  , _strength :: Int
  , _damage :: Int
  , _numCarrying :: Int
  , _nticksNotFed :: Int
  , tickCount :: Int
  , _events :: StateEvents
  , _enemyDir :: Direction
  , _action :: AntAction
  , _mode :: AntMode
  } deriving (Eq)

makeLenses ''StateEvents

makeLenses ''AntState

defaultAntState =
  AntState
  { id = 0
  , teamID = 0
  , _pos = V2 0 0
  , _dist = V2 0 0
  , _dir = V2 0 0
  , lastDirChange = 0
  , _strength = 0
  , _damage = 0
  , _numCarrying = 0
  , _nticksNotFed = 0
  , tickCount = 0
  , _events = defaultStateEvents
  , _enemyDir = V2 0 0
  , _action = DoMove
  , _mode = Scouting
  }

updateInitAntState :: Int -> AntState -> AntState
updateInitAntState tickCount state@AntState {_events} =
  state
  { tickCount
  , _damage = 0
  , _events = _events {_enemy = False, _food = False, _attacked = False}
  }

isAlive :: Optic' (->) (Const Bool) AntState Bool
isAlive = mode ./= Dead

hasDir :: Optic' (->) (Const Bool) AntState Bool
hasDir = dir ./= (V2 0 0)

nextPos :: Optic' (->) (Const Position) AntState Position
nextPos = pos .+. dir

isCarryCapacityReached :: Optic' (->) (Const Bool) AntState Bool
isCarryCapacityReached = numCarrying .>=. strength

isCarrying :: Optic' (->) (Const Bool) AntState Bool
isCarrying = numCarrying `gtL` 0

canAttack :: Optic' (->) (Const Bool) AntState Bool
canAttack = isCarrying . to not .&&. hasDir