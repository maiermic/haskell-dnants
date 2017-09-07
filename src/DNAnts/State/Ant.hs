module DNAnts.State.Ant where

import DNAnts.Client (Client)
import DNAnts.State.AntState (AntState)
import DNAnts.Types (Position)
import SDL.Vect (V2)

data AntTeam = AntTeam
  { teamID :: Int
  , teamSize :: Int
  , client :: Client
  , ants :: [Ant]
  , spawnPoints :: [V2 Int]
  , numFood :: Int
  }

data Ant = Ant
  { team :: AntTeam
  , state :: AntState
  }