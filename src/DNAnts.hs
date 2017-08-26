{-# LANGUAGE NamedFieldPuns #-}

module DNAnts
  ( runApp
  ) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import DNAnts.View.Window (Window(Window, renderer), runWindow)
import Data.DNAnts.AppSettings
       (AppSettings(AppSettings, framesPerSecond, gridExtends,
                    gridSpacing))
import Data.Text (pack)
import GHC.Word (Word8)
import qualified SDL
import SDL (($=))
import qualified SDL.Raw
import SDL.Vect (V2(V2), V4(V4))

defer = flip finally

rgb :: Word8 -> Word8 -> Word8 -> V4 Word8
rgb r g b = V4 r g b maxBound

render Window {renderer} = do
  SDL.rendererDrawColor renderer $= rgb 211 211 211
  SDL.clear renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  SDL.present renderer

gameLoop settings window lastFrameTime = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` events
  unless quit $ do
    frameTime <- SDL.Raw.getTicks
    let deltaTime = frameTime - lastFrameTime
        minFrameTime = fromIntegral (1000 `div` framesPerSecond settings)
    render window
    frameTimeAfter <- SDL.Raw.getTicks
    when (frameTimeAfter - frameTime < minFrameTime) $
      SDL.delay $ minFrameTime - (frameTimeAfter - frameTime)
    gameLoop settings window frameTime

runApp :: String -> AppSettings -> IO ()
runApp title settings@AppSettings {gridExtends, gridSpacing} =
  let (gridWidth, gridHeight) = gridExtends
      windowWidth = fromIntegral (gridWidth * gridSpacing)
      windowHeight = fromIntegral (gridHeight * gridSpacing)
  in runWindow (pack title) windowWidth windowHeight $ \window ->
       gameLoop settings window =<< SDL.Raw.getTicks