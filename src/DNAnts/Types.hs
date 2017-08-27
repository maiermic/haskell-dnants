module DNAnts.Types where

import GHC.Word (Word8)
import SDL.Vect (V4(V4))

data Orientation
  = None
  | North
  | South
  | East
  | West
  | NorthEast
  | SouthEast
  | SouthWest
  | NorthWest

type Point = (Int, Int)

type Position = (Int, Int)

type Direction = (Int, Int)

type Extents = (Int, Int)

defaultExtents :: Extents
defaultExtents = (0, 0)

type Color = V4 Word8

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = V4 r g b maxBound

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba = V4

data AppSettings = AppSettings
  { gridExtends :: Extents
  , gridSpacing :: Int
  , framesPerSecond :: Int
  , roundsPerSecond :: Int
  , traceRounds :: Int
  , initTeamSize :: Int
  , numTeams :: Int
  , showGrid :: Bool
  , showTraces :: Bool
  , teamCodes :: [String]
  } deriving (Show)