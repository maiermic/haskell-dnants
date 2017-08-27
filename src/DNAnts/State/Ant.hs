module DNAnts.State.Ant where

import DNAnts.Client (Client)
import DNAnts.State.AntState (AntState)
import DNAnts.Types (Position)

data AntTeam = AntTeam
  { teamID :: Int
  , teamSize :: Int
  , client :: Client
  , ants :: [Ant]
  , spawnPoints :: [Position]
  , numFood :: Int
  }

data Ant = Ant
  { team :: AntTeam
  , state :: AntState
  }