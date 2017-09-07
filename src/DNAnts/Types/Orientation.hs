{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.Types.Orientation where

import SDL.Vect (V2(V2))

data Orientation =
  Orientation (Maybe LongitudeOrientation)
              (Maybe LatitudeOrientation)

data LongitudeOrientation
  = North
  | South

data LatitudeOrientation
  = East
  | West

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

noOrientation :: Orientation
noOrientation = Orientation Nothing Nothing

directionOfOrientation :: Orientation -> V2 Int
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
  in V2 lonDir latDir