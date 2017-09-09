{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.State.GameState where

import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Extra
import DNAnts.Client
import Data.Maybe
import Data.Foldable
import Control.Lens.Traversal
import Control.Lens
       ((<>=), (%=), (+=), (-=), (.=), (^?), ix, makeLenses, to, use, view, Contravariant, Profunctor, Optic', LensLike', preuse)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.State.Lazy (StateT, get, execStateT)
import DNAnts.Debug (debugShow)
import DNAnts.Lens
import DNAnts.State.Ant as AT -- TODO move AntTeam to own module
import DNAnts.State.Ant as A
import DNAnts.State.Ant
import DNAnts.State.AntId as AI
import DNAnts.State.AntState
import DNAnts.State.AntState as AS
import DNAnts.State.Cell
import DNAnts.State.CellState as CS
import DNAnts.State.Grid
import DNAnts.State.Map
import DNAnts.State.Population (Population)
import DNAnts.Types
import DNAnts.Types.Orientation
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
  , _attacks :: [Attack]
  }

data Attack = Attack
  { _offender :: AntState
  , _defender :: Ant
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
  traverseAntStatesNested updatePosition
  traverseAntStatesNested updateAction
  handleAttacks
  traverseAntStatesNested updateReaction

handleAttacks :: StateT GameState IO ()
handleAttacks = do
  as <- use attacks
  forM_ as $ \Attack {_offender, _defender} -> do
    let defenderId = AS.id $ A._state _defender
    traverseAntStates %= \antState ->
      if AS.id antState == defenderId
        then antState {_damage = _damage antState + _strength _offender}
        else antState
  attacks .= []

traverseAntStates ::
     Applicative f => (AntState -> f AntState) -> GameState -> f GameState
traverseAntStates = populFront . traverse . ants . traverse . AT.state

traverseAntStatesNested ::
     Monad m
  => StateT AntState (StateT [Ant] (StateT GameState m)) ()
  -> StateT GameState m ()
traverseAntStatesNested fn =
  populFront <<~% do zoom ants $ Prelude.id <<~% zoom AT.state fn

type NestedAntState' m a = StateT AntState (StateT [Ant] (StateT GameState m)) a
type NestedAntState m = NestedAntState' m ()

liftGameState :: (Monad (t m), Monad m, MonadTrans t, MonadTrans t1) =>
     m a -> t1 (t m) a
liftGameState = lift . lift

-- TODO refactor
takeFoodNL :: Monad m => NestedAntState' m Int
takeFoodNL = do
  p <- use AS.pos
  Just consumed <-
    liftGameState $ preuse $ gridCellStateL p . CS.numFood . to (min 1)
  liftGameState $ gridCellStateL p . CS.numFood -= consumed
  return consumed

dieNL :: Monad m => NestedAntState m
dieNL = do
  mode .= Dead
  -- TODO leave cell

dropFoodNL :: Monad m => Int -> NestedAntState m
dropFoodNL dropAmount = do
  p <- use AS.pos
  liftGameState $ gridCellStateL p . CS.numFood -= dropAmount

gridCellStateL ::  Applicative f => Position -> LensLike' f GameState CellState
gridCellStateL (V2 x y) = gridFront . cells . cellAtL x y . cellStateL

updateAction :: MonadIO m => NestedAntState m
updateAction = do
  gameState <- liftGameState get
  whenL isAlive $ do
    events . food .=. cellOfAnt (gridState gameState) . isFoodCell
    scanForEnemies gameState
    updateAntState
    applyActions

updateReaction :: MonadIO m => NestedAntState m
updateReaction =
  whenL isAlive $ do
    AntState {_damage, _numCarrying, _strength} <- get
    when (_strength <= _damage) $ do
      if _strength > 1
      then strength .= max 0 (_strength - _damage `div` _strength)
      else dieNL
      when (_numCarrying > 0) $ do
        dropFoodNL _numCarrying
        numCarrying .= 0

applyActions :: Monad m => NestedAntState m
applyActions = do
  use action >>= \case
    DoEat -> eat
    DoHarvest -> harvest
    DoAttack -> attack
    _ -> return ()

eat :: Monad m => NestedAntState m
eat = do
  consumed <- takeFoodNL
  when (consumed > 0) $ do
    nticksNotFed .= 0
    strength %= \s -> min 10 (s + consumed)


harvest :: Monad m => NestedAntState m
harvest =
  unlessL isCarryCapacityReached $ do
    consumed <- takeFoodNL
    when (consumed > 0) $ numCarrying += consumed

attack :: Monad m => NestedAntState m
attack = do
  whenL canAttack $ do
    GameState {_gridFront, _populFront} <- liftGameState get
    _offender@AntState {_pos, teamID} <- get
    whenJust (findAliveAdjEnemy teamID _pos _gridFront _populFront) $ \enemy ->
      addAttack Attack {_offender, _defender = enemy}

addAttack :: Monad m => Attack -> NestedAntState m
addAttack attack = liftGameState $ attacks <>= [attack]

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
      A.numFood .=> debugShow "numFood"
      AT.teamSize .=> debugShow "AT.teamSize"
      unlessL (ants . to length .>=. AT.teamSize) $ do
        whenL (A.numFood .>= 8) $ do
          AT.teamSize += 1
          A.numFood -= 8
        let (Just baseCell) = grid ^? cells . cellAtL x y
        when (isCellTaken baseCell) $ addAntAt spawnPoint
        addAntAt spawnPoint

addAntAt :: Position -> StateT AntTeam IO ()
addAntAt pos = do
  team <- get
  ants %= \ants' -> ants' ++ [createAnt team (length ants') pos]
  ants .=>> debugShow "ants" . length

updatePosition :: MonadIO m => NestedAntState m
updatePosition = do
  mode .=> liftIO . debugShow "mode"
  whenL isAlive $ do
    nticksNotFed += 1
    action .=> liftIO . debugShow "action"
    use action >>= \case
      DoMove -> move
      _ -> action .= DoIdle

move :: MonadIO m => NestedAntState m
move = whenL (isAlive .&&. hasDir) $ do
  pos' <- use nextPos
  gameState <- liftGameState get
  if allowsMoveTo pos' (gridState gameState)
  then do
    events . collision .= False
    dist .+=. dir
    -- TODO leave and enter cell
    AS.pos .= pos'
  else onCollision

turnDir :: MonadIO m => Int -> StateT AntState m ()
turnDir 0 = return ()
turnDir turn =
  dir %= \dir' ->
    let ortIdx = turn + (or2int $ dir2or dir')
    in or2dir $ int2or $ clampOrientation ortIdx

onCollision :: MonadIO m => NestedAntState m
onCollision = events . collision .= True

-- TODO solve cyclic import to be able to move this to client
updateAntState :: MonadIO m => NestedAntState m
updateAntState = simpleClient

simpleClient :: MonadIO m => NestedAntState m
simpleClient = do
  AntState {_mode, _events, tickCount} <- get
  let StateEvents {_collision, _attacked, _food, _enemy} = _events
  case _mode of
    Scouting -> do
      if _collision
        then turnDir 1
        else if tickCount `mod` 6 == 0
               then turnDir 1
               else when (tickCount `mod` 3 == 0) $ turnDir (-1)
      move
    Eating -> eat
    Harvesting -> harvest
    Homing -> move
    _ -> return ()