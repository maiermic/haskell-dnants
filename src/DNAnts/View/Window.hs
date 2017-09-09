{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.View.Window where

import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import DNAnts.Defer
import Data.Text (Text)
import Foreign.C.Types (CInt)
import qualified SDL
import SDL.Vect (V2(V2))

data Window = Window
  { window :: SDL.Window
  , renderer :: SDL.Renderer
  }

getWindow :: MonadIO m => Text -> CInt -> CInt -> ExceptT e (DeferM m) SDL.Window
getWindow title width height = do
  window <-
    liftIO $
    SDL.createWindow
      title
      SDL.defaultWindow
      { SDL.windowPosition = SDL.Centered
      , SDL.windowInitialSize = V2 width height
      }
  deferE $ SDL.destroyWindow window
  return window

getRenderer :: MonadIO m => SDL.Window -> ExceptT e (DeferM m) SDL.Renderer
getRenderer window = do
  renderer <-
    liftIO $
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = True
      }
  deferE $ SDL.destroyRenderer renderer
  return renderer