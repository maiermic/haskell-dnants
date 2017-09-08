{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DNAnts.State.Input where

import qualified SDL

data Input
  = Reset
  | Quit
  deriving (Eq, Show)

toInput :: SDL.Keycode -> Maybe Input
toInput =
  \case
    SDL.KeycodeEscape -> Just Reset
    SDL.KeycodeQ -> Just Quit
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