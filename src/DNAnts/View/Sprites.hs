{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.View.Sprites where

import Codec.Picture.Types (Image(Image, imageHeight, imageWidth))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.DNAnts.ResourceM
       (ResourceM, onReleaseResources)
import DNAnts.SVG (SVG, createSurfaceFromSVG, loadSVGImage)
import qualified SDL
import SDL.Vect (V2(V2))

type Sprite = SDL.Texture

data Sprites = Sprites
  { rock :: Sprite
  }

loadSprites :: MonadIO m => SDL.Renderer -> ResourceM m Sprites
loadSprites renderer = do
  rock <- loadSprite renderer "assets/rock.svg"
  return Sprites {rock}

loadSprite :: MonadIO m => SDL.Renderer -> FilePath -> ResourceM m Sprite
loadSprite renderer filePath = do
  svg <- liftIO $ loadSVGImage filePath
  -- TODO Nothing case
  case svg of
    Just image@Image {imageWidth, imageHeight} -> do
      surface <-
        liftIO $
        createSurfaceFromSVG
          image
          (V2 (fromIntegral imageWidth) (fromIntegral imageHeight))
      texture <- liftIO $ SDL.createTextureFromSurface renderer surface
      liftIO $ SDL.freeSurface surface
      onReleaseResources $ SDL.destroyTexture texture
      return texture