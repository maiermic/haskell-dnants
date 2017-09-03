{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DNAnts.State.Map where

import Control.Lens
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import DNAnts.State.Cell (Cell(Cell), defaultCell)
import DNAnts.State.CellState
       (CellType(Barrier, Food, Plain), cellType, defaultCellState)
import DNAnts.State.Grid
       (Grid(Grid, _cells, _extents), defaultGrid)
import qualified DNAnts.State.Grid as G
import DNAnts.State.Population (Population(Population))
import DNAnts.Types (Extents, defaultExtents)

data MapConfig = MapConfig
  { extents :: Extents
  , numGrassRegions :: Int
  , numFoodRegions :: Int
  , numBarriers :: Int
  , numTeams :: Int
  , teamSize :: Int
  , symmetric :: Bool
  }

defaultMapConfig =
  MapConfig
  { extents = defaultExtents
  , numGrassRegions = 0
  , numFoodRegions = 0
  , numBarriers = 0
  , numTeams = 0
  , teamSize = 0
  , symmetric = False
  }

data Map = Map
  { grid :: Grid
  , population :: Population
  }

generateMap :: MapConfig -> IO Map
generateMap config@MapConfig {extents} = do
  grid <- generateGrid extents
  population <- generatePopulation
  return Map {grid, population}

initialGrid :: Extents -> c -> GridCells c
initialGrid (gridWidth, gridHeight) cell =
  replicate gridHeight $ replicate gridWidth cell

type GridCells c = [[c]]

emptyGridCells :: GridCells c
emptyGridCells = []

generateGrid :: Extents -> IO Grid
generateGrid _extents = do
  _cells <- execStateT (generateGridCells _extents) emptyGridCells
  return Grid {_cells, _extents}

this :: Lens' a a
this = lens id $ flip const

cellOfType :: CellType -> Cell
cellOfType cellType = Cell defaultCellState {cellType}

generateGridCells :: Extents -> StateT (GridCells Cell) IO ()
generateGridCells extents@(w, h) = do
  this .= initialGrid extents (cellOfType Plain)
  dropL (h `div` 2) . traverse . dropL (w `div` 2) . traverse .= cellOfType Food
  area 2 1 4 3 . traverse .= cellOfType Barrier

generatePopulation :: IO Population
generatePopulation = return $ Population []

{- |
Grid of indeces as cells

>>> gridIndices 2 3
[[(0,0),(1,0)],[(0,1),(1,1)],[(0,2),(1,2)]]

-}
gridIndices :: (Num t, Num t1, Enum t, Enum t1) => t1 -> t -> [[(t1, t)]]
gridIndices w h = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

{- |
Add indices to grid cells.

>>> addGridIndices [[0..2],[3..5]]
[[(0,(0,0)),(1,(1,0)),(2,(2,0))],[(3,(0,1)),(4,(1,1)),(5,(2,1))]]

-}
addGridIndices :: GridCells a -> GridCells (a, (Int, Int))
addGridIndices g = zipGridCells (,) g (gridIndices 10 10)

{- |
Access cells in an area of the grid.

>>> :{
gridIndices 10 10 ^. area 3 1 4 2
:}
[(3,1),(4,1),(5,1),(6,1),(3,2),(4,2),(5,2),(6,2)]

-}
area ::
     Applicative f
  => Int
  -> Int
  -> Int
  -> Int
  -> ([a] -> f [a])
  -> [[a]]
  -> f [[a]]
area x y w h = dropL y . takeL h . traverse . dropL x . takeL w

{- |
Restrict the context of a lens to the part of a list after @n@ elements.

>>> [0..9] ^. dropL 7
[7,8,9]

>>> [0..5] & dropL 3 . traverse %~ (* 2)
[0,1,2,6,8,10]

-}
dropL :: Int -> Lens' [a] [a]
dropL d = lens (drop d) (\s v -> take d s ++ v)

{- |
Restrict the context of a lens to the first @n@ elements.

>>> [0..9] ^. takeL 3
[0,1,2]

>>> [0..5] & takeL 3 . traverse %~ (+ 10)
[10,11,12,3,4,5]

-}
takeL :: Int -> Lens' [a] [a]
takeL d = lens (take d) (\s v -> v ++ drop d s)

{- |
Access cell at a specific point in the grid.

>>> [[0..2],[3..5]] ^? cellAt 2 1
Just 5

>>> [[0..2],[3..5]] & cellAt 2 1 %~ (* 2)
[[0,1,2],[3,4,10]]

-}
cellAt ::
     (Ixed (IxValue m), Ixed m, Applicative f)
  => Index (IxValue m)
  -> Index m
  -> (IxValue (IxValue m) -> f (IxValue (IxValue m)))
  -> m
  -> f m
cellAt x y = ix y . ix x

{- |
Zip to grids of cells using a mapping function.

>>> :{
zipGridCells
    (\num text -> show num ++ [text])
    [[0..2], [3..5]]
    [['a'..'c'], ['d'..'f']]
:}
[["0a","1b","2c"],["3d","4e","5f"]]

>>> :{
zipGridCells
    (\num text -> show num ++ [text])
    [[0..4], [5..9]]
    [['a'..'c'], ['d'..'f']]
:}
[["0a","1b","2c"],["5d","6e","7f"]]

-}
zipGridCells :: (a -> b -> c) -> GridCells a -> GridCells b -> GridCells c
zipGridCells fn = zipWith (zipWith fn)

{- |
Replace the beginning of the second list with the first list.
The result has at most as much elements as the second list.

>>> replace [6..7] [0..4]
[6,7,2,3,4]

>>> replace [0..4] [5..9]
[0,1,2,3,4]

>>> replace [0..4] [8..9]
[0,1]

-}
replace :: [a] -> [a] -> [a]
replace (x:xs) (_:ys) = x : replace xs ys
replace [] ys = ys
replace _ [] = []

{- |
Overwrite the beginning of the second list with the first list.
The result always contains all values of the first list.

>>> overwrite [6..7] [0..4]
[6,7,2,3,4]

>>> overwrite [0..4] [5..9]
[0,1,2,3,4]

>>> overwrite [0..4] [8..9]
[0,1,2,3,4]

-}
overwrite :: [a] -> [a] -> [a]
overwrite (x:xs) (_:ys) = x : overwrite xs ys
overwrite [] ys = ys
overwrite xs [] = xs