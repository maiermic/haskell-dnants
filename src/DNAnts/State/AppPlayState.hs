{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DNAnts.State.AppPlayState where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import DNAnts.Debug
import DNAnts.State.Ant hiding (teamSize)
import DNAnts.State.AntState
import DNAnts.State.Cell (Cell(Cell), defaultCell)
import DNAnts.State.CellState
       (CellState(CellState, _amount, _cellType),
        CellType(Barrier, Food, Grass, None, Plain, SpawnPoint, Water),
        defaultCellState)
import DNAnts.State.GameState
       (GameState(GameState, _attacks, _gridFront, _populFront,
                  _roundCount, appSettings, gridBack, gridExtents, nteams,
                  populBack),
        gridState)
import DNAnts.State.Grid
       (Grid(Grid, _cells, _extents), gridHeight, gridWidth, indexedCells)
import DNAnts.State.Map
       (Map(Map, grid, population),
        MapConfig(extents, numBarriers, numFoodRegions, numGrassRegions,
                  numTeams, symmetric, teamSize),
        defaultMapConfig, generateMap)
import DNAnts.State.Population (Population)
import DNAnts.Types as AS
import DNAnts.Types
       (AppSettings(AppSettings, _framesPerSecond, gridExtents,
                    _gridSpacing, initTeamSize, numTeams),
        Color, Extents, Position, divA, rect, rgb, rgba, toColor3)
import DNAnts.Types.Orientation
       (Orientation, directionOfOrientation, noOrientation, or2deg, dir2or)
import qualified DNAnts.Types.Orientation as Orientation
import DNAnts.View.Sprites
import DNAnts.View.Window (Window(Window, renderer, window))
import Data.Foldable (forM_)
import Foreign.C.Types (CInt)
import GHC.Word (Word32)
import qualified SDL
import SDL (($=))
import SDL.Vect (Point(P), V2(V2))

-- TODO _app
data AppPlayState = AppPlayState
  { active :: Bool
  , _paused :: Bool
  , _step :: Bool
  , _showCommands :: Bool
  , _showInTraces :: Bool
  , _showOutTraces :: Bool
  , _lastRoundMs :: Word32
  , gridSpacing :: Int
  , gridExtents :: Extents
  , _markedCell :: Position
  , _gameState :: GameState
  , sprites :: Sprites
  , teamColors :: [Color]
  , mapRgbMode :: Color
  , highlightColor :: Color
  , blockedColor :: Color
  , takenColor :: Color
  , foodColor :: Color
  , grassColor :: Color
  }

makeLenses ''AppPlayState

createMapConfig :: AppSettings -> MapConfig
createMapConfig AppSettings {numTeams, gridExtents, initTeamSize} =
  defaultMapConfig
  { extents = gridExtents
  , numGrassRegions = 8
  , numFoodRegions = numTeams * 2
  , numBarriers = 3
  , numTeams
  , teamSize = initTeamSize
  , symmetric = numTeams > 1
  }

createGameState :: AppSettings -> IO GameState
createGameState appSettings@AppSettings {gridExtents, numTeams} = do
  Map {grid, population} <- generateMap $ createMapConfig appSettings
  return
    GameState
    { appSettings
    , nteams = numTeams
    , gridExtents
    , _roundCount = 0
    , _gridFront = grid
    , gridBack = undefined -- TODO
    , _populFront = population
    , populBack = undefined -- TODO
    , _attacks = []
    }

defaultAppPlayState :: AppSettings -> Sprites -> IO AppPlayState
defaultAppPlayState appSettings@AppSettings {gridExtents, _gridSpacing, numTeams} sprites = do
  _gameState <- createGameState appSettings
  return
    AppPlayState
    { active = True
    , _paused = False
    , _step = False
    , _showCommands = True
    , _showInTraces = True
    , _showOutTraces = True
    , _lastRoundMs = 0
    , gridSpacing = _gridSpacing
    , gridExtents
    , _markedCell = V2 (-1) (-1)
    , _gameState
    , sprites
    , teamColors =
        [ rgba 0xff 0x12 0x66 0x88
        , rgba 0x00 0xaa 0x23 0x88
        , rgba 0x87 0x57 0xe8 0x88
        , rgba 0xFF 0xD7 0x00 0x88
        ]
    , mapRgbMode = rgba 0x00 0x00 0x00 0x00
    , highlightColor = rgba 0xaf 0x12 0x12 0xff
    , blockedColor = rgba 0xff 0xb9 0x47 0xff
    , takenColor = rgba 0x23 0x45 0x45 0xff
    , foodColor = rgba 0xfa 0xb7 0x05 0xff
    , grassColor = rgba 0xaa 0xde 0x87 0xff
    }

resetAppPlayState :: AppSettings -> AppPlayState -> IO AppPlayState
resetAppPlayState settings AppPlayState {sprites} =
  defaultAppPlayState settings sprites

draw :: AppSettings -> Window -> AppPlayState -> IO ()
draw settings window@Window {renderer} state = do
  SDL.rendererDrawColor renderer $= rgba 0xc3 0xc3 0xc3 0x00
  SDL.clear renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  renderMap settings window state
  renderObjects settings window state
  -- TODO show grid
  -- TODO render highlight cell
  -- TODO render statusbar
  -- TODO render commands overlay
  SDL.present renderer

renderMap :: AppSettings -> Window -> AppPlayState -> IO ()
renderMap settings window AppPlayState {_gameState, sprites} =
  forM_ (indexedCells $ gridState _gameState) $ \(Cell cellState, pos) ->
    case _cellType cellState of
      Barrier -> renderCell settings window (rock sprites) pos
      Food -> renderFoodCell settings window sprites pos cellState
      _ -> mempty
    -- TODO show traces

renderFoodCell ::
     AppSettings -> Window -> Sprites -> Position -> CellState -> IO ()
renderFoodCell settings window sprites pos cellState =
  let amountLeft = _amount cellState
      amountMax = 4
      amountQurt = (((amountLeft * 100) `div` amountMax) `div` 25) + 1
      sprite =
        case amountQurt of
          1 -> sugah1 sprites
          2 -> sugah2 sprites
          3 -> sugah3 sprites
          _ -> sugah4 sprites
  in when (amountLeft > 0 && amountQurt > 0) $ renderCell settings window sprite pos

renderCell :: AppSettings -> Window -> Sprite -> Position -> IO ()
renderCell AppSettings {_gridSpacing} window texture cellPos =
  let size = _gridSpacing
  in renderTexture window texture ((* _gridSpacing) <$> cellPos) (V2 size size)

renderTexture :: Window -> Sprite -> Position -> Extents -> IO ()
renderTexture Window {renderer} texture (V2 x y) (V2 w h) = do
  let dstRect =
        rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  SDL.copy renderer texture Nothing (Just dstRect)

renderCellWithOrientation :: AppSettings -> Window -> Sprite -> Position -> Orientation -> IO ()
renderCellWithOrientation AppSettings {_gridSpacing} window texture cellPos orientation =
  let size = _gridSpacing
  in renderTextureWithOrientation window texture ((* _gridSpacing) <$> cellPos) (V2 size size) orientation

renderTextureWithOrientation :: Window -> Sprite -> Position -> Extents -> Orientation -> IO ()
renderTextureWithOrientation Window {renderer} texture (V2 x y) (V2 w h) orientation = do
  let dstRect =
        rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
      angle = or2deg orientation
  SDL.rendererDrawBlendMode renderer $= SDL.BlendMod
  SDL.copyEx renderer texture Nothing (Just dstRect) angle Nothing (V2 False False)

renderObjects :: AppSettings -> Window -> AppPlayState -> IO ()
renderObjects settings@AppSettings {_gridSpacing} window AppPlayState { _gameState
                                                                     , sprites
                                                                     , teamColors
                                                                     } =
  let teams = _populFront _gameState
  in forM_ (zip teamColors teams) $
     uncurry $ \teamColor AntTeam {_spawnPoints, _ants} -> do
       forM_ _spawnPoints $ \spawnPoint ->
         drawCellCircle
           window
           (fromIntegral <$> spawnPoint)
           (fromIntegral _gridSpacing)
           noOrientation
           teamColor
       forM_ _ants $ renderAnt settings window sprites teamColor

type LineSegment = (Point V2 CInt, Point V2 CInt)

drawCellCircle :: Window -> V2 CInt -> CInt -> Orientation -> Color -> IO ()
drawCellCircle Window {renderer} pos gridSpacing ornt color = do
  let radiusOuter :: CInt
      radiusOuter = gridSpacing - 1
      radiusOuterV :: V2 CInt
      radiusOuterV =
        fromIntegral <$> directionOfOrientation ornt * fromIntegral radiusOuter
      radiusInner = gridSpacing - 3
      gridSpacingV :: V2 CInt
      gridSpacingV = fromIntegral gridSpacing
      (V2 centerX centerY) =
        pos * gridSpacingV + gridSpacingV `divA` 2 + radiusOuterV
      getLinePoints :: CInt -> [Point V2 CInt]
      getLinePoints r =
        let x0 = centerX - r + 1
            x1 = centerX - (r `div` 2) - 1
            x2 = centerX
            x3 = centerX + (r `div` 2) + 1
            x4 = centerX + r - 1
            y0 = centerY - r + 1
            y1 = centerY - (r `div` 2) - 1
            y2 = centerY
            y3 = centerY + (r `div` 2) + 1
            y4 = centerY + r - 1
        in map P $
           [ V2 x2 y0 -- center top
           , V2 x3 y1
           , V2 x4 y2 -- right center
           , V2 x3 y3
           , V2 x2 y4 -- center
           , V2 x1 y3
           , V2 x0 y2 -- left center
           , V2 x1 y1
           , V2 x2 y0 -- center top
           ]
      lineSegments :: [LineSegment]
      lineSegments =
        concatMap (toLineSegment . getLinePoints) $
        reverse [radiusInner .. radiusOuter]
  SDL.rendererDrawColor renderer $= color
  forM_ lineSegments $ uncurry $ SDL.drawLine renderer

toLineSegment :: [Point V2 CInt] -> [LineSegment]
toLineSegment points = zip points $ tail points

renderAnt :: AppSettings -> Window -> Sprites -> Color -> Ant -> IO ()
renderAnt settings window sprites color Ant {_state} =
  when (_state ^. isAlive) $ do
    let p = _pos _state
    when (_attacked $ _events _state) $
      renderCell settings window (evtAttacked sprites) p
    when (_enemy $ _events _state) $
      renderCell settings window (evtEnemy sprites) p
    when (_strength _state > 0) $ do
      let strengthQurt =
            (((_strength _state * 100) `div` antMaxStrength) `div` 25) + 1
          antSprite = case strengthQurt of
            1 -> ant1 sprites
            2 -> ant2 sprites
            3 -> ant3 sprites
            _ -> ant4 sprites
      SDL.textureColorMod antSprite $= toColor3 color
      renderCellWithOrientation settings window antSprite p $ dir2or $ _dir _state
    when (_state ^. isCarrying) $
      let gsp = AS._gridSpacing settings
          size = gsp
          center = (* gsp) <$> p
          mouthOffset = (* (gsp `div` 4)) <$> _dir _state
          carryingPos = center + mouthOffset
      in renderTexture window (sugah2 sprites) carryingPos (V2 size size)
    when (DoAttack == _action _state) $
      renderCell settings window (actAttack sprites) p