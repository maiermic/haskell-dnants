{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module DNAnts.State.AppPlayState where

import DNAnts.State.Cell (Cell(Cell), defaultCell)
import DNAnts.State.CellState
       (CellState(CellState, cellType),
        CellType(Barrier, Food, Grass, None, Plain, SpawnPoint, Water),
        defaultCellState)
import DNAnts.State.GameState
       (GameState(GameState, appSettings, gridBack, gridExtents,
                  gridFront, nteams, populBack, populFront, roundCount),
        gridState)
import DNAnts.State.Grid
       (Grid(Grid, _cells, _extents), gridHeight, gridWidth, indexedCells)
import DNAnts.Types
       (AppSettings(AppSettings, framesPerSecond, gridExtents,
                    gridSpacing, numTeams),
        Color, Position, rect, rgb, rgba)
import DNAnts.View.Sprites (Sprite, Sprites(Sprites, rock, sugah1))
import DNAnts.View.Window (Window(Window, renderer, window))
import Data.Foldable (forM_)
import Foreign.C.Types (CInt)
import GHC.Word (Word32)
import qualified SDL
import SDL (($=))

-- TODO _app
data AppPlayState = AppPlayState
  { active :: Bool
  , paused :: Bool
  , step :: Bool
  , showCommands :: Bool
  , showInTraces :: Bool
  , showOutTraces :: Bool
  , lastRoundMs :: Word32
  , gridSpacing :: Int
  , gridExtents :: (Int, Int)
  , markedCell :: (Int, Int)
  , gameState :: GameState
  , sprites :: Sprites
  , teamColors :: [Color]
  , mapRgbMode :: Color
  , highlightColor :: Color
  , blockedColor :: Color
  , takenColor :: Color
  , foodColor :: Color
  , grassColor :: Color
  }

defaultAppPlayState :: AppSettings -> Sprites -> AppPlayState
defaultAppPlayState appSettings@AppSettings {gridExtents, gridSpacing, numTeams} sprites =
  let (gridWidth, gridHeight) = gridExtents
      barrierCell = Cell defaultCellState {cellType = Barrier}
      foodCell = Cell defaultCellState {cellType = Food}
      cellRow :: [Cell]
      cellRow = concat $ replicate (gridWidth `div` 2) [barrierCell, foodCell]
      _cells :: [[Cell]]
      _cells = replicate gridHeight $ cellRow
  in AppPlayState
     { active = True
     , paused = False
     , step = False
     , showCommands = True
     , showInTraces = True
     , showOutTraces = True
     , lastRoundMs = 0
     , gridSpacing
     , gridExtents
     , markedCell = (-1, -1)
     , gameState =
         GameState
         { appSettings
         , nteams = numTeams
         , gridExtents
         , roundCount = 0
         , gridFront = Grid {_extents = gridExtents, _cells}
         , gridBack = undefined -- TODO
         , populFront = undefined -- TODO
         , populBack = undefined -- TODO
         }
     , sprites
     , teamColors =
         [ rgba 0xff 0x12 0x66 0x88
         , rgba 0x00 0xaa 0x23 0x88
         , rgba 0x87 0x57 0xe8 0x88
         , rgba 0x84 0xa8 0x36 0x88
         ]
     , mapRgbMode = rgba 0x00 0x00 0x00 0x00
     , highlightColor = rgba 0xaf 0x12 0x12 0xff
     , blockedColor = rgba 0xff 0xb9 0x47 0xff
     , takenColor = rgba 0x23 0x45 0x45 0xff
     , foodColor = rgba 0xfa 0xb7 0x05 0xff
     , grassColor = rgba 0xaa 0xde 0x87 0xff
     }

draw :: AppSettings -> Window -> AppPlayState -> IO ()
draw settings window@Window {renderer} state = do
  SDL.rendererDrawColor renderer $= rgba 0xc3 0xc3 0xc3 0x00
  SDL.clear renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  renderMap settings window state
  SDL.present renderer

renderMap :: AppSettings -> Window -> AppPlayState -> IO ()
renderMap settings window AppPlayState {gameState, sprites} =
  forM_ (indexedCells $ gridState gameState) $ \(Cell CellState {cellType}, (x, y)) ->
    case cellType of
      Barrier -> renderCell settings window (rock sprites) (x, y)
      Food -> renderCell settings window (sugah1 sprites) (x, y)
      _ -> mempty

renderCell :: AppSettings -> Window -> Sprite -> Position -> IO ()
renderCell AppSettings {gridSpacing} Window {renderer} texture (cellX, cellY) = do
  let size = (fromIntegral gridSpacing) :: CInt
      dstRect =
        rect (fromIntegral cellX * size) (fromIntegral cellY * size) size size
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  SDL.copy renderer texture Nothing (Just dstRect)