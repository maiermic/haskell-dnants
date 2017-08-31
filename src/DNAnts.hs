{-# LANGUAGE NamedFieldPuns #-}

module DNAnts
  ( runApp
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Lazy
       (StateT, execStateT, get, put)
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
import GHC.Word (Word32, Word8)
import qualified SDL
import qualified SDL.Raw

type FrameTime = Word32

gameLoop :: StateT (AppSettings, Window, FrameTime) IO ()
gameLoop = do
  (settings, window, lastFrameTime) <- get
  events <- liftIO $ map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` events
  unless quit $ do
    frameTime <- liftIO SDL.Raw.getTicks
    let deltaTime = frameTime - lastFrameTime
        minFrameTime = fromIntegral (1000 `div` framesPerSecond settings)
    frameTimeAfter <-
      liftIO $ do
        draw settings window defaultAppPlayState
        SDL.Raw.getTicks
    when (frameTimeAfter - frameTime < minFrameTime) $
      liftIO $ SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)
    put (settings, window, frameTime)
    gameLoop

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
       initialFrameTime <- liftIO SDL.Raw.getTicks
       liftIO $
         execStateT
           gameLoop
           (settings, Window {renderer, window}, initialFrameTime)
       return ()