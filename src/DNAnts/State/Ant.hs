{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.Ant where

import Control.Lens (makeLenses)
import DNAnts.Client (Client)
import DNAnts.State.AntState
import DNAnts.State.AntState as AS
import DNAnts.Types (Position)
import SDL.Vect (V2)

data Ant = Ant
  { team :: AntTeam
  , _state :: AntState
  }

data AntTeam = AntTeam
  { teamID :: Int
  , _teamSize :: Int
  , client :: Client
  , _ants :: [Ant]
  , _spawnPoints :: [V2 Int]
  , _numFood :: Int
  }

makeLenses ''Ant
makeLenses ''AntTeam

createAnt :: AntTeam -> Int -> V2 Int -> Ant
createAnt team@AntTeam {teamID} id _pos =
  Ant {team, _state = defaultAntState {AS.id = id, teamID, _pos, _strength = 5}}