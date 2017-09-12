{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.AppEngine where

import Control.Lens (makeLenses, to, preuse, use, (%=))
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad (Monad, mapM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)
import Control.Monad.Trans.Except
import DNAnts.Defer
import Control.Exception.Safe
import DNAnts.Debug
import DNAnts.Lens
       ((.=.), (.=>), (.=>>), (<~%), getsM, unlessL, whenL)
import DNAnts.State.AppPlayState
import DNAnts.State.AppPlayState as APS
import DNAnts.State.CellState as CS
import DNAnts.State.AntId
import DNAnts.State.GameState
import DNAnts.State.Input
import DNAnts.Types
       (AppSettings(AppSettings, _framesPerSecond, _roundsPerSecond,
                    gridExtents, _gridSpacing),
        framesPerSecond, maySpeedDown, maySpeedUp, rgb, roundsPerSecond,
        showGrid, showTraces, Position, gridSpacing)
import DNAnts.Types as AS
import DNAnts.View.Sprites (loadSprites)
import DNAnts.View.Window
       (Window(Window, renderer, window), getRenderer, getWindow)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import GHC.Word (Word32, Word8)
import Lens.Family2.State.Lazy (zoom)
import SDL.Vect
import qualified SDL
import qualified SDL.Raw

type FrameTime = Word32

data AppEngine = AppEngine
  { _settings :: AppSettings
  , _state :: AppPlayState
  , _window :: Window
  , _isRunning :: Bool
  }

makeLenses ''AppEngine

handleEvents :: StateT AppEngine IO ()
handleEvents = do
  events <- liftIO $ map SDL.eventPayload <$> SDL.pollEvents
  when (SDL.QuitEvent `elem` events) $ isRunning .= False
  handleKeyboardEvents events
  handleMouseEvents events
  -- TODO mark cell on mouse button down

handleMouseEvents :: [SDL.EventPayload] -> StateT AppEngine IO ()
handleMouseEvents events =
  mapM_ handleInput $ mapMaybe inputFromMouseEvent $
  mapMaybe toMouseButtonEventData events

handleKeyboardEvents :: [SDL.EventPayload] -> StateT AppEngine IO ()
handleKeyboardEvents events =
  mapM_ handleInput $ mapMaybe toInput $ mapMaybe toPressedKey events

handleInput :: Input -> StateT AppEngine IO ()
handleInput =
  \case
    Quit -> isRunning .= False
    Reset -> do
      settings' <- use settings
      state <~% resetAppPlayState settings'
    Help -> state . showCommands %= not
    ToggleGrid -> settings . showGrid %= not
    ToggleTraces -> settings . showTraces %= not
    ToggleInTraces -> state . showInTraces %= not
    ToggleOutTraces -> state . showOutTraces %= not
    SpeedUp ->
      whenL (settings . to maySpeedUp) $ settings . roundsPerSecond += 1
    SpeedDown ->
      whenL (settings . to maySpeedDown) $ settings . roundsPerSecond -= 1
    MinSpeed -> settings . roundsPerSecond .= 1
    MaxSpeed -> settings . roundsPerSecond .=. settings . framesPerSecond
    Pause ->
      zoom state $ do
        paused %= not
        unlessL paused $ markedCell .= V2 (-1) (-1)
        showCommands .= False
    SingleStep -> do
      state . step .= True
      state . paused .= False
    ShowDebugInfoOfPosition pos -> showDebugInfoOfPosition pos

showDebugInfoOfPosition :: Position -> StateT AppEngine IO ()
showDebugInfoOfPosition pos =
  whenL (state . paused) $ do
    gridSpacing <- use (settings . AS.gridSpacing)
    let clickedCellPos = (`div` gridSpacing) <$> pos
    liftIO $ putStrLn $ "clicked grid at " ++ show clickedCellPos
    state . markedCell .= clickedCellPos
    logCell clickedCellPos

logCell :: Position -> StateT AppEngine IO ()
logCell pos@(V2 x y) = do
  cellStateM <- preuse $ state . gameState . gridCellStateL pos
  case cellStateM of
    Nothing ->
      liftIO $ putStrLn $ "cell at postition " ++ show pos ++ " not found"
    Just cellState -> do
      liftIO $ putStrLn $ show cellState
      case CS._antID cellState of
        Nothing -> return ()
        Just antId -> do
          ant <- use $ state . gameState . populFront . to (getAntById antId)
          liftIO $ putStrLn $ show ant

  return ()

without :: (Eq a, Foldable t) => [a] -> t a -> [a]
without values excludes = filter (`notElem` excludes) values

update :: StateT AppEngine IO ()
update = do
  AppEngine {_settings, _state = AppPlayState {_lastRoundMs}} <- get
  let msPerRound = 1000 `div` fromIntegral (_roundsPerSecond _settings)
  ms <- SDL.Raw.getTicks
  when (ms - _lastRoundMs >= msPerRound) $ do
    zoom state $ do
      lastRoundMs .= ms
      unlessL paused $ zoom gameState nextGameState
      whenL step $ do
        paused .= True
        step .= False

gameLoop :: StateT AppEngine IO ()
gameLoop = do
  handleEvents
  whenL isRunning $ do
    update
    getsM drawFrame
    gameLoop

runApp :: String -> AppSettings -> IO ()
runApp title _settings@AppSettings {gridExtents, _gridSpacing} =
  let (V2 gridWidth gridHeight) = gridExtents
      windowWidth = fromIntegral (gridWidth * _gridSpacing)
      windowHeight = fromIntegral (gridHeight * _gridSpacing)
  in do
      result <- runDefer $ runExceptT $ do
       liftIO $ SDL.initialize [SDL.InitVideo]
       deferE SDL.quit
       _window <- getWindow (pack title) windowWidth windowHeight
       renderer <- getRenderer _window
       sprites <- loadSprites renderer
       _state <- liftIO $ defaultAppPlayState _settings sprites
       liftIO $
         execStateT
           gameLoop
           AppEngine
           { _settings
           , _window = Window {renderer, window = _window}
           , _state
           , _isRunning = True
           }
       return ()
      handleResult result

handleResult :: Show a => Either SomeException a -> IO ()
handleResult result =
  case result of
    Left e -> putStrLn $ "caught an exception " ++ show e
    Right _ -> putStrLn "clean shutdown"

drawFrame :: AppEngine -> IO ()
drawFrame AppEngine {_settings, _window, _state} = do
  frameTime <- SDL.Raw.getTicks
  draw _settings _window _state
  frameTimeAfter <- SDL.Raw.getTicks
  let minFrameTime = fromIntegral (1000 `div` _framesPerSecond _settings)
  when (frameTimeAfter - frameTime < minFrameTime) $ liftIO $ SDL.delay $
    minFrameTime -
    (frameTimeAfter - frameTime)