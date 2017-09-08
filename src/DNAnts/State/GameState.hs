{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.State.GameState where

import Control.Lens
       ((%=), (+=), (-=), (.=), (^?), ix, makeLenses, to, use)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, get)
import DNAnts.Debug (debugShow)
import DNAnts.Lens
import DNAnts.State.Ant as AT
import DNAnts.State.Ant
import DNAnts.State.AntState
import DNAnts.State.AntState as AS
import DNAnts.State.Cell (isCellTaken)
import DNAnts.State.Grid
import DNAnts.State.Map
import DNAnts.State.Population (Population)
import DNAnts.Types
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
  tickCount <- use roundCount
  traverseAntStates %= updateInitAntState tickCount
  zoom traverseAntStates updatePosition

traverseAntStates ::
     Applicative f => (AntState -> f AntState) -> GameState -> f GameState
traverseAntStates = populFront . traverse . ants . traverse . AT.state

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
    forM_ sps $ \spawnPoint@(V2 x y) -> do
      numFood .=> debugShow "numFood"
      AT.teamSize .=> debugShow "AT.teamSize"
      unlessL (ants . to length .>=. AT.teamSize) $ do
        whenL (numFood .>= 8) $ do
          AT.teamSize += 1
          numFood -= 8
        let (Just baseCell) = grid ^? cells . cellAtL x y
        when (isCellTaken baseCell) $ addAntAt spawnPoint
        addAntAt spawnPoint

addAntAt :: Position -> StateT AntTeam IO ()
addAntAt pos = do
  team <- get
  ants %= \ants' -> ants' ++ [createAnt team (length ants') pos]
  ants .=>> debugShow "ants" . length

updatePosition :: StateT AntState IO ()
updatePosition = do
  mode .=> debugShow "mode"
  whenL isAlive $ do
    nticksNotFed += 1
    action .=> debugShow "action"
    use action >>= \case
      DoMove -> move
      _ -> action .= DoIdle

move :: StateT AntState IO ()
move = whenL (isAlive .&&. hasDir) $ do
  pos' <- use nextPos
  let grid = undefined -- TODO
  if allowsMoveTo pos' grid
  then do
    events . collision .= False
    dist .+=. dir
    -- TODO leave and enter cell
    AS.pos .= pos'
  else onCollision

onCollision :: StateT AntState IO ()
onCollision = events . collision .= True