{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.State.GameState where

import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Extra
import Data.Maybe
import Data.Foldable
import Control.Lens.Traversal
import Control.Lens
       ((%=), (+=), (-=), (.=), (^?), ix, makeLenses, to, use, view)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.State.Lazy (StateT, get)
import DNAnts.Debug (debugShow)
import DNAnts.Lens
import DNAnts.State.Ant as AT
import DNAnts.State.Ant
import DNAnts.State.AntId as AI
import DNAnts.State.AntState
import DNAnts.State.AntState as AS
import DNAnts.State.Cell
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
  traverseAntStatesNested updateAction

traverseAntStates ::
     Applicative f => (AntState -> f AntState) -> GameState -> f GameState
traverseAntStates = populFront . traverse . ants . traverse . AT.state

traverseAntStatesNested ::
     Monad m
  => StateT AntState (StateT [Ant] (StateT GameState m)) ()
  -> StateT GameState m ()
traverseAntStatesNested fn =
  populFront <<~% do zoom ants $ Prelude.id <<~% zoom AT.state fn

type NestedAntState m = StateT AntState (StateT [Ant] (StateT GameState m)) ()

liftGameState :: (Monad (t m), Monad m, MonadTrans t, MonadTrans t1) =>
     m a -> t1 (t m) a
liftGameState = lift . lift

updateAction :: MonadIO m => NestedAntState m
updateAction = do
  gameState <- liftGameState get
  whenL isAlive $ do
    events . food .=. cellOfAnt (gridState gameState) . isFoodCell
    scanForEnemies gameState
    -- TODO request next state
    -- TODO apply action

scanForEnemies :: Monad m => GameState -> StateT AntState m ()
scanForEnemies GameState {_gridFront, _populFront} = do
  AntState {_pos, teamID} <- get
  whenJust (findAliveAdjEnemy teamID _pos _gridFront _populFront) onEnemy

onEnemy :: Monad m => Ant -> StateT AntState m ()
onEnemy Ant {_state = AntState {_pos = enemyPos}} = do
  enemyDir .=. AS.pos . to (enemyPos -)
  events . enemy .= True

findAliveAdjEnemy :: Int -> Position -> Grid -> Population -> Maybe Ant
findAliveAdjEnemy antTeamId pos grid population =
  find (view (AT.state . isAlive)) $ adjEnemies antTeamId pos grid population

getTeamById :: Int -> Population -> Maybe AntTeam
getTeamById i population = find ((== i) . AT.teamID) population

getAntById :: AntId -> Population -> Maybe Ant
getAntById AntId {_id, _teamId} population =
  case getTeamById _teamId population of
    Nothing -> Nothing
    Just team -> find ((== _id) . AS.id . _state) $ _ants team

adjEnemies :: Int -> Position -> Grid -> Population -> [Ant]
adjEnemies antTeamId pos grid population =
  mapMaybe (`getAntById` population) $ adjEnemyIds antTeamId pos grid

adjEnemyIds :: Int -> Position -> Grid -> [AntId]
adjEnemyIds antTeamId pos grid =
  filter ((/= antTeamId) . AI._teamId) $ adjAntIds pos grid

adjAntIds :: Position -> Grid -> [AntId]
adjAntIds pos grid =
  mapMaybe (antIdOfCell . flip cellAt (_cells grid)) $ adjPositions pos grid

adjPositions :: Position -> Grid -> [Position]
adjPositions pos grid = filter (`containsPosition` grid) $ map (+ pos) adjDir

{- |
Directions to adjacent grid positions.

>>> adjDir
[V2 (-1) (-1),V2 0 (-1),V2 1 (-1),V2 (-1) 0,V2 1 0,V2 (-1) 1,V2 0 1,V2 1 1]

-}
adjDir :: [Direction]
adjDir = concat [[V2 x y | x <- [-1 .. 1], V2 x y /= V2 0 0] | y <- [-1 .. 1]]

isFoodCell = isNotSpawnPoint .&&. containsFood

cellOfAnt Grid {_cells} = AS.pos . to (`cellAt` _cells)

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