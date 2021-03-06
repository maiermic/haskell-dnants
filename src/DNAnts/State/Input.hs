{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DNAnts.State.Input where

import qualified SDL
import DNAnts.Types (Position)

data Input
  = Reset
  | Help
  | Quit
  | ToggleGrid
  | ToggleTraces
  | ToggleInTraces
  | ToggleOutTraces
  | SpeedUp
  | SpeedDown
  | MinSpeed
  | MaxSpeed
  | Pause
  | SingleStep
  | ShowDebugInfoOfPosition Position
  deriving (Eq, Show)

toInput :: SDL.Keycode -> Maybe Input
toInput =
  \case
    SDL.KeycodeEscape -> Just Reset
    SDL.KeycodeH -> Just Help
    SDL.KeycodeQ -> Just Quit
    SDL.KeycodeG -> Just ToggleGrid
    SDL.KeycodeT -> Just ToggleTraces
    SDL.KeycodeI -> Just ToggleInTraces
    SDL.KeycodeO -> Just ToggleOutTraces
    SDL.KeycodeRight -> Just SpeedUp
    SDL.KeycodeLeft -> Just SpeedDown
    SDL.KeycodeDown -> Just MinSpeed
    SDL.KeycodeUp -> Just MaxSpeed
    SDL.KeycodeSpace -> Just Pause
    SDL.KeycodeN -> Just SingleStep
    _ -> Nothing

isPressedKey :: SDL.KeyboardEventData -> Bool
isPressedKey eventData = SDL.Pressed == SDL.keyboardEventKeyMotion eventData

toPressedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toPressedKey (SDL.KeyboardEvent e)
  | isPressedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
toPressedKey _ = Nothing

isReleasedKey :: SDL.KeyboardEventData -> Bool
isReleasedKey eventData = SDL.Released == SDL.keyboardEventKeyMotion eventData

toReleasedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toReleasedKey (SDL.KeyboardEvent e)
  | isReleasedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
toReleasedKey _ = Nothing

toMouseButtonEventData :: SDL.EventPayload -> Maybe SDL.MouseButtonEventData
toMouseButtonEventData (SDL.MouseButtonEvent e) = Just e
toMouseButtonEventData _ = Nothing

isMouseButtonPressed :: SDL.MouseButtonEventData -> Bool
isMouseButtonPressed eventData =
  SDL.mouseButtonEventMotion eventData == SDL.Pressed

isLeftMouseButton :: SDL.MouseButtonEventData -> Bool
isLeftMouseButton eventData =
  SDL.mouseButtonEventButton eventData == SDL.ButtonLeft

isLeftMouseButtonPressed :: SDL.MouseButtonEventData -> Bool
isLeftMouseButtonPressed e = isMouseButtonPressed e && isLeftMouseButton e

inputFromMouseEvent :: SDL.MouseButtonEventData -> Maybe Input
inputFromMouseEvent e | isLeftMouseButtonPressed e =
  let (SDL.P pos) = SDL.mouseButtonEventPos e
  in Just $ ShowDebugInfoOfPosition $ fromIntegral <$> pos
inputFromMouseEvent _ = Nothing

