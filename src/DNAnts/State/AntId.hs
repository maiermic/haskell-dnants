{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.AntId where

import Control.Lens

data AntId = AntId
  { _teamId :: Int
  , _id :: Int
  } deriving (Show)

makeLenses ''AntId