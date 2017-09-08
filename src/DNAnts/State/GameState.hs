{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.GameState where

import DNAnts.Debug (debugShow)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((+=), makeLenses, use, to, (%=), ix)
import DNAnts.Lens (whenL, (.=>>), (.=>))
import Control.Monad.Trans.State.Lazy (StateT, get)
import DNAnts.State.Ant
import DNAnts.State.Grid
import DNAnts.State.Map
import DNAnts.State.Population (Population)
import DNAnts.Types (AppSettings, Extents)
import Lens.Family2.State.Lazy (zoom)
import SDL.Vect (V2(V2))

data GameState = GameState
  { appSettings :: AppSettings
  , nteams :: Int
  , gridExtents :: Extents
  , _roundCount :: Int
  , _gridFront :: Grid
  , gridBack :: Grid
  , _populFront :: Population
  , populBack :: Population
  }

makeLenses ''GameState

gridState :: GameState -> Grid
gridState = _gridFront

nextGameState :: StateT GameState IO ()
nextGameState = do
  roundCount += 1
  roundCount .=> debugShow "roundCount"
  updatePopulation
  zoom gridFront updateGrid

updatePopulation :: StateT GameState IO ()
updatePopulation = do
  updateAntTeams

updateAntTeams :: StateT GameState IO ()
updateAntTeams = whenL (roundCount . to isSpawnRound) spawnAnts

isSpawnRound :: Int -> Bool
isSpawnRound rc = rc `mod` 20 == 1

spawnAnts :: StateT GameState IO ()
spawnAnts = do
  liftIO $ putStrLn "spawn ants"
  zoom (populFront . traverse) $ do
    (V2 x y):_ <- use spawnPoints
--      baseCell <- use $ gridFront . cellAtL x y
--      whenL (gridFront . cellAtL x y . to isTaken) $
        -- TODO if (_ants.size() >= _team_size) { return; }
--        addAntAt x y
    addAntAt x y


addAntAt :: Int -> Int -> StateT AntTeam IO ()
addAntAt x y = do
  team <- get
  ants %= \ants' -> ants' ++ [createAnt team (length ants') (x, y)]
  ants .=>> debugShow "ants" . length
