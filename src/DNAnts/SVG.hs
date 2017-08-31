{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.SVG where

import Codec.Picture.Types
       (Image(imageData, imageWidth), PixelRGBA8)
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

loadSVGImage :: FilePath -> IO (Maybe SVG)
loadSVGImage filepath = do
  mdoc <- loadSvgFile filepath
  case mdoc of
    Nothing -> return Nothing
    Just doc -> do
      cache <- loadCreateFontCache "fonty-texture-cache"
      (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
      return $ Just finalImage