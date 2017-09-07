{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.AppEngine
  ( runApp
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Lazy
       (StateT, execStateT, get, put)
import Control.Monad.Writer.DNAnts.ResourceM
       (ResourceM, onReleaseResources, runResourceM)
import DNAnts.State.AppPlayState
       (AppPlayState, defaultAppPlayState, draw)
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtents,
                    gridSpacing),
        rgb)
import DNAnts.View.Window
       (Window(Window, renderer, window), getRenderer, getWindow)
import Data.Text (pack)
import GHC.Word (Word32, Word8)
import qualified SDL
import qualified SDL.Raw
import DNAnts.View.Sprites (loadSprites)

type FrameTime = Word32

gameLoop :: StateT (AppSettings, Window, AppPlayState) IO ()
gameLoop = do
  (settings, window, state) <- get
  events <- liftIO $ map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` events
  unless quit $ do
    frameTime <- liftIO SDL.Raw.getTicks
    let minFrameTime = fromIntegral (1000 `div` framesPerSecond settings)
    frameTimeAfter <-
      liftIO $ do
        draw settings window state
        SDL.Raw.getTicks
    when (frameTimeAfter - frameTime < minFrameTime) $
      liftIO $ SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)
    put (settings, window, state)
    gameLoop

runApp :: String -> AppSettings -> IO ()
runApp title settings@AppSettings {gridExtents, gridSpacing} =
  let (gridWidth, gridHeight) = gridExtents
      windowWidth = fromIntegral (gridWidth * gridSpacing)
      windowHeight = fromIntegral (gridHeight * gridSpacing)
  in runResourceM $ do
       liftIO $ SDL.initialize [SDL.InitVideo]
       onReleaseResources SDL.quit
       window <- getWindow (pack title) windowWidth windowHeight
       renderer <- getRenderer window
       sprites <- loadSprites renderer
       state <- liftIO $ defaultAppPlayState settings sprites
       liftIO $
         execStateT
           gameLoop
           (settings, Window {renderer, window}, state)
       return ()