{-# LANGUAGE NamedFieldPuns #-}

module DNAnts
  ( runApp
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.DNAnts.ResourceM
       (ResourceM, onReleaseResources, runResourceM)
import DNAnts.State.AppPlayState (defaultAppPlayState, draw)
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtends,
                    gridSpacing),
        rgb)
import DNAnts.View.Window
       (Window(Window, renderer, window), getRenderer, getWindow)
import Data.Text (pack)
import GHC.Word (Word8)
import qualified SDL
import qualified SDL.Raw

gameLoop settings window lastFrameTime = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` events
  unless quit $ do
    frameTime <- SDL.Raw.getTicks
    let deltaTime = frameTime - lastFrameTime
        minFrameTime = fromIntegral (1000 `div` framesPerSecond settings)
    draw settings window defaultAppPlayState
    frameTimeAfter <- SDL.Raw.getTicks
    when (frameTimeAfter - frameTime < minFrameTime) $
      SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)
    gameLoop settings window frameTime

runApp :: String -> AppSettings -> IO ()
runApp title settings@AppSettings {gridExtends, gridSpacing} =
  let (gridWidth, gridHeight) = gridExtends
      windowWidth = fromIntegral (gridWidth * gridSpacing)
      windowHeight = fromIntegral (gridHeight * gridSpacing)
  in runResourceM $ do
       liftIO $ SDL.initialize [SDL.InitVideo]
       onReleaseResources SDL.quit
       window <- getWindow (pack title) windowWidth windowHeight
       renderer <- getRenderer window
       liftIO $ gameLoop settings Window {renderer, window} =<< SDL.Raw.getTicks