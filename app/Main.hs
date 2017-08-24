module Main where

import Data.DNAnts.AppSettings
       (AppSettings(AppSettings, framesPerSecond, gridExtends,
                    gridSpacing, initTeamSize, numTeams, roundsPerSecond, showGrid,
                    showTraces, teamCodes, traceRounds))

defaultAppSettings :: AppSettings
defaultAppSettings =
  AppSettings
  { gridExtends = (23, 23)
  , gridSpacing = 32
  , framesPerSecond = 30
  , roundsPerSecond = 4
  , traceRounds = 300
  , initTeamSize = 5
  , numTeams = 0
  , showGrid = False
  , showTraces = False
  , teamCodes = []
  }

main :: IO ()
main = print $ show defaultAppSettings