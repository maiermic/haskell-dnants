{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.Types where

import Control.Applicative (liftA2)
import Control.Lens (makeLenses)
import Foreign.C.Types (CInt)
import GHC.Word (Word8)
import qualified SDL
import SDL.Vect

type Position = V2 Int

type Direction = V2 Int

type Extents = V2 Int

type Region = SDL.Rectangle Int

defaultExtents :: Extents
defaultExtents = V2 0 0

-- | 'uncurry' converts a curried function to a function on vectors.
uncurryV2 :: (a -> a -> b) -> (V2 a -> b)
uncurryV2 f (V2 x y) = f x y

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
  , _framesPerSecond :: Int
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
maySpeedUp AppSettings {_roundsPerSecond, _framesPerSecond} =
  _roundsPerSecond < _framesPerSecond

maySpeedDown :: AppSettings -> Bool
maySpeedDown AppSettings {_roundsPerSecond} = _roundsPerSecond > 1

rect :: a -> a -> a -> a -> SDL.Rectangle a
rect x y w h = SDL.Rectangle (P $ V2 x y) (V2 w h)

divA :: (Integral b, Applicative f) => f b -> f b -> f b
divA v1 v2 = div <$> v1 <*> v2