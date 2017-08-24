module Data.DNAnts.AppSettings where

data AppSettings = AppSettings
  { gridExtends :: (Int, Int)
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