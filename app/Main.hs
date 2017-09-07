module Main where

import Control.Monad (when)
import DNAnts.AppEngine (runApp)
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtents,
                    gridSpacing, initTeamSize, numTeams, roundsPerSecond, showGrid,
                    showTraces, teamCodes, traceRounds))
import System.Console.GetOpt
       (ArgDescr(NoArg, OptArg, ReqArg), ArgOrder(RequireOrder),
        OptDescr(Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

defaultAppSettings :: AppSettings
defaultAppSettings =
  AppSettings
  { gridExtents = (23, 23)
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

options :: [OptDescr (AppSettings -> IO AppSettings)]
options =
  [ Option
      "g"
      ["grid"]
      (ReqArg
         (\arg settings -> return settings {gridExtents = (read arg, read arg)})
         "size")
      "Size of the grid (equal width and height)"
  , Option
      "w"
      ["grid-w"]
      (ReqArg
         (\arg settings@AppSettings {gridExtents = (_, height)} ->
            return settings {gridExtents = (read arg, height)})
         "width")
      "Width of the grid"
  , Option
      "h"
      ["grid-h"]
      (ReqArg
         (\arg settings@AppSettings {gridExtents = (width, _)} ->
            return settings {gridExtents = (width, read arg)})
         "height")
      "Height of the grid"
  , Option
      "r"
      ["trace-rounds"]
      (ReqArg
         (\arg settings -> return settings {traceRounds = read arg})
         "rounds")
      "Trace rounds"
  , Option
      "t"
      ["team"]
      (ReqArg
         (\arg settings ->
            return
              settings
              { teamCodes = teamCodes settings ++ [arg]
              , numTeams = numTeams settings + 1
              })
         "team-code")
      "Add a team (multiple teams can be added)"
  , Option
      "s"
      ["team-size"]
      (ReqArg
         (\arg settings -> return settings {initTeamSize = read arg})
         "size")
      "Set the initial team size"
  , Option
      "b"
      ["big"]
      (NoArg (\settings -> return settings {gridSpacing = 32}))
      "Show big grid"
  ]

-- TODO use_32px
processArgs :: [String] -> IO AppSettings
processArgs args =
  case getOpt RequireOrder options args of
    (actions, _, []) -> foldl (>>=) (return defaultAppSettings) actions
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: dnants [OPTION...]"

validateSettings :: AppSettings -> IO ()
validateSettings settings =
  when (length (teamCodes settings) > 4) $
  ioError $ userError "Only a maximum of 4 teams is allowed"

main :: IO ()
main = do
  settings <- getArgs >>= processArgs
  validateSettings settings
  runApp "game'-._.of'._.survive" settings