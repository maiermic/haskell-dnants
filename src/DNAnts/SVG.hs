{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.SVG where

import Codec.Picture.Types
       (Image(imageData, imageWidth), PixelRGBA8)
import Control.Exception.Safe
       (Exception, MonadThrow, SomeException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Generic (thaw)
import Data.Vector.Storable.Mutable (IOVector)
import Graphics.Rasterific.Svg
       (loadCreateFontCache, renderSvgDocument)
import Graphics.Svg (loadSvgFile)

import Data.Word (Word8)

import Data.Vector.Storable (Vector)

import Foreign.C.Types (CInt)
import qualified SDL
import SDL.Vect (V2(V2))

type SVG = Image PixelRGBA8

createSurfaceFromSVG :: SVG -> V2 CInt -> IO SDL.Surface
createSurfaceFromSVG image surfaceSize = do
  let rawImageData :: Vector Word8
      rawImageData = imageData image
      imWidth :: Int
      imWidth = imageWidth image
      pitch :: CInt
      pitch = 4 * fromIntegral imWidth
  mutableVector <- convertToMutableVector rawImageData
  SDL.createRGBSurfaceFrom mutableVector surfaceSize pitch SDL.ABGR8888

convertToMutableVector :: Vector Word8 -> IO (IOVector Word8)
convertToMutableVector = thaw

data LoadSVGImageException =
  LoadSVGImageException String
  deriving (Show)

instance Exception LoadSVGImageException

loadSVGImage :: (MonadIO m, MonadThrow m) => FilePath -> m SVG
loadSVGImage filepath = do
  mdoc <- liftIO $ loadSvgFile filepath
  case mdoc of
    Nothing ->
      throwM $
      LoadSVGImageException $
      "Ressource " ++ show filepath ++ " failed to load from drive"
    Just doc ->
      liftIO $ do
        cache <- loadCreateFontCache "fonty-texture-cache"
        (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
        return finalImage