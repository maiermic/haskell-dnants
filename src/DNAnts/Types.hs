{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.Types where

import Control.Applicative (liftA2)
import Control.Lens (makeLenses)
import Foreign.C.Types (CInt)
import GHC.Word (Word8)
import qualified SDL
import SDL.Vect (Point(P), V2(V2), V3(V3), V4(V4))

type Point = (Int, Int)

type Position = (Int, Int)

type Direction = (Int, Int)

type Extents = (Int, Int)

type Region = SDL.Rectangle Int

defaultExtents :: Extents
defaultExtents = (0, 0)

type Color = V4 Word8

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = V4 r g b maxBound

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba = V4

type Color3 = V3 Word8

toColor3 :: Color -> Color3
toColor3 (V4 r g b a) = V3 r g b

data AppSettings = AppSettings
  { gridExtents :: Extents
  , gridSpacing :: Int
  , framesPerSecond :: Int
  , _roundsPerSecond :: Int
  , traceRounds :: Int
  , initTeamSize :: Int
  , numTeams :: Int
  , _showGrid :: Bool
  , _showTraces :: Bool
  , teamCodes :: [String]
  } deriving (Show)

makeLenses ''AppSettings

maySpeedUp :: AppSettings -> Bool
maySpeedUp AppSettings {_roundsPerSecond, framesPerSecond} =
  _roundsPerSecond < framesPerSecond

rect :: a -> a -> a -> a -> SDL.Rectangle a
rect x y w h = SDL.Rectangle (P $ V2 x y) (V2 w h)

divA :: (Integral b, Applicative f) => f b -> f b -> f b
divA v1 v2 = div <$> v1 <*> v2