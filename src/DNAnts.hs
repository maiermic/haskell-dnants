{-# LANGUAGE NamedFieldPuns #-}

module DNAnts
  ( runApp
  ) where

import Control.Exception (finally)
import DNAnts.View.Window (runWindow)
import Data.DNAnts.AppSettings
       (AppSettings(AppSettings, gridExtends, gridSpacing))
import Data.Text (pack)
import qualified SDL
import SDL.Vect (V2(V2))

defer = flip finally

runApp :: String -> AppSettings -> IO ()
runApp title settings@AppSettings {gridExtends, gridSpacing} =
  let (gridWidth, gridHeight) = gridExtends
      windowWidth = fromIntegral (gridWidth * gridSpacing)
      windowHeight = fromIntegral (gridHeight * gridSpacing)
  in runWindow (pack title) windowWidth windowHeight $ \window -> do
       SDL.delay 3000000
       SDL.quit