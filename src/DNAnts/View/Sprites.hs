{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.View.Sprites where

import Codec.Picture.Types (Image(Image, imageHeight, imageWidth))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import DNAnts.Defer
import DNAnts.SVG (SVG, createSurfaceFromSVG, loadSVGImage)
import qualified SDL
import SDL.Vect (V2(V2))

type Sprite = SDL.Texture

data Sprites = Sprites
  { commands :: Sprite
  , rock :: Sprite
  , sugah1 :: Sprite
  , sugah2 :: Sprite
  , sugah3 :: Sprite
  , sugah4 :: Sprite
  , ant1 :: Sprite
  , ant2 :: Sprite
  , ant3 :: Sprite
  , ant4 :: Sprite
  , evtAttacked :: Sprite
  , evtEnemy :: Sprite
  , actAttack :: Sprite
  , icoPlay :: Sprite
  , icoPause :: Sprite
  , num0 :: Sprite
  , num1 :: Sprite
  , num2 :: Sprite
  , num3 :: Sprite
  , num4 :: Sprite
  , num5 :: Sprite
  , num6 :: Sprite
  , num7 :: Sprite
  , num8 :: Sprite
  , num9 :: Sprite
  }

loadSprites :: MonadIO m => SDL.Renderer -> ExceptT e (DeferM m) Sprites
loadSprites renderer = do
  commands <- loadSprite renderer "assets/commands.svg"
  rock <- loadSprite renderer "assets/rock.svg"
  sugah1 <- loadSprite renderer "assets/sugah-1.svg"
  sugah2 <- loadSprite renderer "assets/sugah-2.svg"
  sugah3 <- loadSprite renderer "assets/sugah-3.svg"
  sugah4 <- loadSprite renderer "assets/sugah-4.svg"
  ant1 <- loadSprite renderer "assets/ant-1.svg"
  ant2 <- loadSprite renderer "assets/ant-2.svg"
  ant3 <- loadSprite renderer "assets/ant-3.svg"
  ant4 <- loadSprite renderer "assets/ant-4.svg"
  evtAttacked <- loadSprite renderer "assets/evt_attacked.svg"
  evtEnemy <- loadSprite renderer "assets/evt_enemy.svg"
  actAttack <- loadSprite renderer "assets/act_attack.svg"
  icoPlay <- loadSprite renderer "assets/ico_play.svg"
  icoPause <- loadSprite renderer "assets/ico_pause.svg"
  num0 <- loadSprite renderer "assets/num_0.svg"
  num1 <- loadSprite renderer "assets/num_1.svg"
  num2 <- loadSprite renderer "assets/num_2.svg"
  num3 <- loadSprite renderer "assets/num_3.svg"
  num4 <- loadSprite renderer "assets/num_4.svg"
  num5 <- loadSprite renderer "assets/num_5.svg"
  num6 <- loadSprite renderer "assets/num_6.svg"
  num7 <- loadSprite renderer "assets/num_7.svg"
  num8 <- loadSprite renderer "assets/num_8.svg"
  num9 <- loadSprite renderer "assets/num_9.svg"
  return
    Sprites
    { commands
    , rock
    , sugah1
    , sugah2
    , sugah3
    , sugah4
    , ant1
    , ant2
    , ant3
    , ant4
    , evtAttacked
    , evtEnemy
    , actAttack
    , icoPlay
    , icoPause
    , num0
    , num1
    , num2
    , num3
    , num4
    , num5
    , num6
    , num7
    , num8
    , num9
    }

loadSprite ::
     MonadIO m => SDL.Renderer -> FilePath -> ExceptT e (DeferM m) Sprite
loadSprite renderer filePath = do
  image@Image {imageWidth, imageHeight} <- liftIO $ loadSVGImage filePath
  surface <-
    liftIO $
    createSurfaceFromSVG
      image
      (V2 (fromIntegral imageWidth) (fromIntegral imageHeight))
  texture <- liftIO $ SDL.createTextureFromSurface renderer surface
  liftIO $ SDL.freeSurface surface
  deferE $ SDL.destroyTexture texture
  return texture