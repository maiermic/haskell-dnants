{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.State.GameState where

import Control.Lens
       (Contravariant, LensLike', Optic', Profunctor, (%=), (+=), (-=),
        (.=), (<>=), (^?), ix, makeLenses, preuse, to, use, view)
import Control.Lens.Traversal
import Control.Monad (forM_, when)
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, get)
import DNAnts.Client
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
import Data.Foldable
import Data.Maybe
import Lens.Family2.State.Lazy (zoom)
import SDL.Vect (V2(V2))
import System.Random

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
  preuse (populFront . to head . AT.numFood) >>= liftIO . putStrLn . ("AT.numFood: " ++) . show

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
  => NestedAntState m
  -> StateT GameState m ()
traverseAntStatesNested fn =
  populFront <<~% ants <<~% zoom AT.state fn

type NestedAntState' m a = StateT AntState (StateT AntTeam (StateT GameState m)) a

type NestedAntState m = NestedAntState' m ()

liftGameState ::
     (Monad (t m), Monad m, MonadTrans t, MonadTrans t1) => m a -> t1 (t m) a
liftGameState = lift . lift

liftAntTeam :: (Monad m, MonadTrans t) => m a -> t m a
liftAntTeam = lift

-- TODO refactor
takeFoodNL :: Monad m => NestedAntState' m Int
takeFoodNL = do
  p <- use AS.pos
  Just consumed <-
    liftGameState $ preuse $ gridCellStateL p . CS.numFood . to (min 1)
  liftGameState $ gridCellStateL p . CS.numFood -= consumed
  return consumed

dieNL :: MonadIO m => NestedAntState m
dieNL = do
  mode .= Dead
  leaveCurrentCell

dropFoodNL :: Monad m => Int -> NestedAntState m
dropFoodNL dropAmount = do
  p <- use AS.pos
  liftGameState $ gridCellStateL p . CS.numFood -= dropAmount

gridCellStateL :: Applicative f => Position -> LensLike' f GameState CellState
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
move =
  whenL (isAlive .&&. hasDir) $ do
    pos' <- use nextPos
    gameState <- liftGameState get
    if allowsMoveTo pos' (gridState gameState)
      then do
        events . collision .= False
        dist .+=. dir
        leaveCurrentCell
        enterNextCell
        AS.pos .= pos'
      else onCollision

enterNextCell :: MonadIO m => NestedAntState m
enterNextCell = do
  isAlive' <- use isAlive
  if not isAlive'
    then nextCellNL $ do
           taken .= False
           CS.antID .= Nothing
    else do
      AntState {id, teamID} <- get
      nextCellNL $ do
        taken .= True
        CS.antID .= (Just $ AntId {_teamId = teamID, _id = id})
    -- TODO add in trace
    -- TODO refactor
      Just CellState {_amount, _cellType} <-
        use AS.nextPos >>= \p -> liftGameState $ preuse $ gridCellStateL p
      when (_amount > 0) onFoodCell
      when (_cellType == SpawnPoint) onHomeCell

onFoodCell :: MonadIO m => NestedAntState m
onFoodCell = events . food .= True

onHomeCell :: MonadIO m => NestedAntState m
onHomeCell =
  whenL isHomeCell $ do
    food <- use numCarrying
    numCarrying .= 0
    liftAntTeam $ AT.storeFood food
    switchMode Scouting

nextCellNL :: MonadIO m => StateT CellState m () -> NestedAntState m
nextCellNL fn =
  use AS.nextPos >>= \p -> liftGameState $ zoom (gridCellStateL p) fn

currentCellNL :: MonadIO m => StateT CellState m () -> NestedAntState m
currentCellNL fn =
  use AS.pos >>= \p -> liftGameState $ zoom (gridCellStateL p) fn

leaveCurrentCell :: MonadIO m => NestedAntState m
leaveCurrentCell =
  currentCellNL $ do
    taken .= False
    CS.antID .= Nothing
  -- TODO add out trace

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
updateAntState = greedyClient

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

randomTurn :: MonadIO m => NestedAntState m
randomTurn = do
  AntState {..} <- get
  whenL (dir .== V2 0 0) $ dir .= V2 0 1
  let r1 = (tickCount - (id * 3)) `mod` 4 < 2
      r2 = (tickCount + id) `mod` 8 < 6
  turnDir $
    if | r1 && r2 -> (-2)
       | r1 -> 2
       | r2 -> -1
       | otherwise -> 1

greedyClient :: MonadIO m => NestedAntState m
greedyClient = do
  rand <- liftIO $ randomRIO (0, 5)
  antState@AntState {..} <- get
  grid <- liftGameState $ use $ to gridState
  let StateEvents {..} = _events
  let isFoodInDirection :: Direction -> Bool
      isFoodInDirection d =
        let cellPos = d + _pos
        in if containsPosition cellPos grid
           then view containsFood $ cellAt cellPos $ _cells grid
           else False
      foodDir =
        fromMaybe _dir $
        find isFoodInDirection adjDir
  liftIO $ putStrLn $ "foodDir = " ++ show foodDir ++ ", " ++ show antState
  case _mode of
    Scouting ->
      if | _enemy ->
           do dir .= _enemyDir
              action .= DoAttack
         | _collision ->
           do turnDir 1
              action .= DoMove
         | _food ->
           if _strength < 5
             then do
               action .= DoEat
               mode .= Eating
             else do
               action .= DoHarvest
               mode .= Harvesting
         | foodDir /= (V2 0 0) ->
           do dir .= foodDir
              action .= DoMove
         | tickCount - lastDirChange > 7 ->
           do turnDir $ ((rand + tickCount) `mod` 3) - 1
              action .= DoMove
         | otherwise -> return ()
    Eating ->
      if | _enemy ->
           do dir .= _enemyDir
              action .= DoAttack
         | _food ->
           if _strength >= 5
             then do
               action .= DoHarvest
               mode .= Harvesting
             else action .= DoEat
         | otherwise ->
           do mode .= Scouting
              action .= DoMove
    Harvesting ->
      if | _food ->
           if _numCarrying < _strength
             then action .= DoHarvest
             else do
               dir .= - (signum <$> _dist)
               mode .= Homing
               action .= DoMove
         | otherwise ->
           if _numCarrying > 0
             then do
               dir .= - (signum <$> _dist)
               mode .= Homing
               action .= DoMove
             else do
               mode .= Scouting
               action .= DoMove
    Homing ->
      if | _collision ->
           do turnDir 1
              action .= DoMove
         | _numCarrying == 0 ->
           do mode .= Scouting
              randomTurn
              action .= DoMove
         | otherwise ->
           do dir .= - (signum <$> _dist)
              action .= DoMove
    _ -> return ()