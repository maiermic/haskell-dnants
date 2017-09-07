{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.AppEngine where

import DNAnts.Lens (getsM, (.=>))
import Control.Lens
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad (Monad, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)
import Control.Monad.Writer.DNAnts.ResourceM
       (ResourceM, onReleaseResources, runResourceM)
import DNAnts.State.AppPlayState
       (AppPlayState, defaultAppPlayState, draw)
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtents,
                    gridSpacing),
        rgb)
import DNAnts.View.Sprites (loadSprites)
import DNAnts.View.Window
       (Window(Window, renderer, window), getRenderer, getWindow)
import Data.Text (pack)
import GHC.Word (Word32, Word8)
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

gameLoop :: StateT AppEngine IO ()
gameLoop = do
  events <- liftIO $ map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` events
  unless quit $ do
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
  frameTime <- liftIO SDL.Raw.getTicks
  draw _settings _window _state
  frameTimeAfter <- SDL.Raw.getTicks
  let minFrameTime = fromIntegral (1000 `div` framesPerSecond _settings)
  when (frameTimeAfter - frameTime < minFrameTime) $
    liftIO $ SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)