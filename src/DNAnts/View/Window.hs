{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.View.Window
  ( runWindow
  , Window(..)
  ) where

import Control.Exception (finally)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import qualified SDL
import SDL.Vect (V2(V2))

data Window = Window
  { window :: SDL.Window
  , renderer :: SDL.Renderer
  }

defer = flip finally

runWindow :: Text -> CInt -> CInt -> (Window -> IO a) -> IO a
runWindow title width height app = do
  SDL.initialize [SDL.InitVideo]
  defer SDL.quit $ do
    window <-
      SDL.createWindow
        title
        SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize = V2 width height
        }
    defer (SDL.destroyWindow window) $ do
      SDL.raiseWindow window
      renderer <-
        SDL.createRenderer
          window
          (-1)
          SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedVSyncRenderer
          , SDL.rendererTargetTexture = True
          }
      defer (SDL.destroyRenderer renderer) $ app Window {window, renderer}