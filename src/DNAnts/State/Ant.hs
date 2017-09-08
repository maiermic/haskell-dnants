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

createAnt :: AntTeam -> Int -> Position -> Ant
createAnt team@AntTeam {teamID} id pos =
  Ant {team, _state = defaultAntState {AS.id = id, teamID, pos, strength = 5}}