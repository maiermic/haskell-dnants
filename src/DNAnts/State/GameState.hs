{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.GameState where

import Control.Lens
       ((%=), (+=), (-=), (^?), ix, makeLenses, to, use)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, get)
import DNAnts.Debug (debugShow)
import DNAnts.Lens ((.=>), (.=>>), (.>=), (.>=.), unlessL, whenL)
import DNAnts.State.Ant as AT
import DNAnts.State.Ant
import DNAnts.State.Cell (isCellTaken)
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
  grid <- use gridFront
  zoom (populFront . traverse) $ do
    sps <- use spawnPoints
    forM_ sps $ \(V2 x y) -> do
      numFood .=> debugShow "numFood"
      AT.teamSize .=> debugShow "AT.teamSize"
      unlessL (ants . to length .>=. AT.teamSize) $ do
        whenL (numFood .>= 8) $ do
          AT.teamSize += 1
          numFood -= 8
        let (Just baseCell) = grid ^? cells . cellAtL x y
        when (isCellTaken baseCell) $ addAntAt x y
        addAntAt x y

addAntAt :: Int -> Int -> StateT AntTeam IO ()
addAntAt x y = do
  team <- get
  ants %= \ants' -> ants' ++ [createAnt team (length ants') (x, y)]
  ants .=>> debugShow "ants" . length