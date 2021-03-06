{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module DNAnts.Types.Orientation where

import Foreign.C.Types (CDouble)
import DNAnts.Types
import Data.List (elemIndex, lookup)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import SDL.Vect (V2(V2))

data Orientation =
  Orientation (Maybe LongitudeOrientation)
              (Maybe LatitudeOrientation)
  deriving (Eq, Show)

data LongitudeOrientation
  = North
  | South
  deriving (Eq, Show)

data LatitudeOrientation
  = East
  | West
  deriving (Eq, Show)

north :: Orientation
north = Orientation (Just North) Nothing

northWest :: Orientation
northWest = Orientation (Just North) (Just West)

northEast :: Orientation
northEast = Orientation (Just North) (Just East)

south :: Orientation
south = Orientation (Just South) Nothing

southWest :: Orientation
southWest = Orientation (Just South) (Just West)

southEast :: Orientation
southEast = Orientation (Just South) (Just East)

east :: Orientation
east = Orientation Nothing (Just East)

west :: Orientation
west = Orientation Nothing (Just West)

noOrientation :: Orientation
noOrientation = Orientation Nothing Nothing

directionOfOrientation :: Orientation -> Direction
directionOfOrientation (Orientation lon lat) =
  let lonDir =
        case lon of
          Nothing -> 0
          Just North -> (-1)
          Just South -> 1
      latDir =
        case lat of
          Nothing -> 0
          Just West -> (-1)
          Just East -> 1
  in V2 latDir lonDir

orientationTable =
  [ (north, 0)
  , (northEast, 1)
  , (east, 2)
  , (southEast, 3)
  , (south, 4)
  , (southWest, 5)
  , (west, 6)
  , (northWest, 7)
  ]

or2int :: Orientation -> Int
or2int o = fromMaybe (-1) $ lookup o orientationTable

dir2or :: Direction -> Orientation
dir2or (V2 x y) =
  let latDir =
        if | x == 0 -> Nothing
           | x < 0 -> Just West
           | x > 0 -> Just East
      lonDir =
        if | y == 0 -> Nothing
           | y < 0 -> Just North
           | y > 0 -> Just South
  in Orientation lonDir latDir

or2dir :: Orientation -> Direction
or2dir = directionOfOrientation

int2or :: Int -> Orientation
int2or i = fromMaybe noOrientation $ lookup i (map swap orientationTable)

or2deg :: Orientation -> CDouble
or2deg o = fromMaybe 0.0 $ lookup o degreeTable

degreeTable :: [(Orientation, CDouble)]
degreeTable =
  [ (north, 0)
  , (northEast, 45)
  , (east, 90)
  , (southEast, 135)
  , (south, 180)
  , (southWest, 225)
  , (west, 270)
  , (northWest, 315)
  ]

clampOrientation :: Int -> Int
clampOrientation o =
  if | o < 0 -> o + 7
     | o > 7 -> 0
     | otherwise -> o