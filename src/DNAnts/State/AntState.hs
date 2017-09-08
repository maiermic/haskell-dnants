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
  , strength :: Int
  , damage :: Int
  , numCarrying :: Int
  , _nticksNotFed :: Int
  , tickCount :: Int
  , _events :: StateEvents
  , enemyDir :: Direction
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
  , strength = 0
  , damage = 0
  , numCarrying = 0
  , _nticksNotFed = 0
  , tickCount = 0
  , _events = defaultStateEvents
  , enemyDir = V2 0 0
  , _action = DoMove
  , _mode = Scouting
  }

updateInitAntState :: Int -> AntState -> AntState
updateInitAntState tickCount state@AntState {_events} =
  state
  { tickCount
  , damage = 0
  , _events = _events {_enemy = False, _food = False, _attacked = False}
  }

isAlive :: Optic' (->) (Const Bool) AntState Bool
isAlive = mode ./= Dead

hasDir :: Optic' (->) (Const Bool) AntState Bool
hasDir = dir ./= (V2 0 0)

nextPos :: Optic' (->) (Const Position) AntState Position
nextPos = pos .+. dir