{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module DNAnts.AppEngine where

import Control.Lens (makeLenses, use)
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad (Monad, mapM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)
import Control.Monad.Writer.DNAnts.ResourceM
       (ResourceM, onReleaseResources, runResourceM)
import DNAnts.Debug
import DNAnts.Lens ((.=>), (.=>>), (<~%), getsM, unlessL, whenL)
import DNAnts.State.AppPlayState
import DNAnts.State.GameState
import DNAnts.State.Input
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtents,
                    gridSpacing, roundsPerSecond),
        rgb, showGrid, showTraces)
import DNAnts.View.Sprites (loadSprites)
import DNAnts.View.Window
       (Window(Window, renderer, window), getRenderer, getWindow)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import GHC.Word (Word32, Word8)
import Lens.Family2.State.Lazy (zoom)
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
    _ -> return ()

without :: (Eq a, Foldable t) => [a] -> t a -> [a]
without values excludes = filter (`notElem` excludes) values

update :: StateT AppEngine IO ()
update = do
  AppEngine {_settings, _state = AppPlayState {_lastRoundMs}} <- get
  let msPerRound = 1000 `div` fromIntegral (roundsPerSecond _settings)
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
runApp title _settings@AppSettings {gridExtents, gridSpacing} =
  let (gridWidth, gridHeight) = gridExtents
      windowWidth = fromIntegral (gridWidth * gridSpacing)
      windowHeight = fromIntegral (gridHeight * gridSpacing)
  in runResourceM $ do
       liftIO $ SDL.initialize [SDL.InitVideo]
       onReleaseResources SDL.quit
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

drawFrame :: AppEngine -> IO ()
drawFrame AppEngine {_settings, _window, _state} = do
  frameTime <- SDL.Raw.getTicks
  draw _settings _window _state
  frameTimeAfter <- SDL.Raw.getTicks
  let minFrameTime = fromIntegral (1000 `div` framesPerSecond _settings)
  when (frameTimeAfter - frameTime < minFrameTime) $
    liftIO $ SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)