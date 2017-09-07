{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DNAnts.State.Map where

import Control.Lens
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad (forM_, replicateM, replicateM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import DNAnts.Lens (this)
import DNAnts.State.Ant
import DNAnts.State.Cell (Cell(Cell), defaultCell)
import DNAnts.State.CellState
       (CellType(Barrier, Food, Plain, SpawnPoint), cellType,
        defaultCellState)
import DNAnts.State.Grid
       (Grid(Grid, _cells, _extents), defaultGrid)
import qualified DNAnts.State.Grid as G
import DNAnts.State.Population (Population(Population))
import DNAnts.Types (Extents, Region, defaultExtents, divA, rect)
import Data.List.Split (chunksOf)

-- TODO only use explicit imports
import Data.Maybe (maybeToList)
import Debug.Trace
import Linear.V2 (V2(V2))
import SDL (Point(P), Rectangle(Rectangle))
import System.Random
       (Random, newStdGen, randomR, randomRIO, randomRs, split)

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
generateMap config = do
  grid <- generateGrid config
  return Map {grid, population = getPopulation config}

{- |
Create grid filled with cells of an initial value.
-}
initialGrid :: Extents -> c -> GridCells c
initialGrid (gridWidth, gridHeight) cell =
  replicate gridHeight $ replicate gridWidth cell

type GridCells c = [[c]]

emptyGridCells :: GridCells c
emptyGridCells = []

generateGrid :: MapConfig -> IO Grid
generateGrid config@MapConfig {extents = _extents} = do
  _cells <- execStateT (generateGridCells config) emptyGridCells
  return Grid {_cells, _extents}

cellOfType :: CellType -> Cell
cellOfType cellType = Cell defaultCellState {cellType}

generateGridCells :: MonadIO m => MapConfig -> StateT (GridCells Cell) m ()
generateGridCells MapConfig { extents
                            , numFoodRegions
                            , numBarriers
                            , symmetric
                            , numTeams
                            } = do
  let (w, h) = extents
  put $ initialGrid extents (cellOfType Plain)
  addFoodRegions numFoodRegions (V2 w h)
  addBarrierCells numBarriers (V2 w h)
  when symmetric makeGridSymmetric
  addSpawnPoints $ getSpawnPoints numTeams (V2 w h)

getSpawnPoints :: Int -> V2 Int -> [V2 Int]
getSpawnPoints numTeams extents =
  let center = extents `divA` 2
  in case numTeams of
       0 -> []
       1 -> [center]
       _ ->
         let spawnCorners =
               take numTeams $ [V2 (-1) (-1), V2 1 1, V2 1 (-1), V2 (-1) 1]
         in map (\corner -> center + corner * extents `divA` 3) spawnCorners

addSpawnPoints :: MonadIO m => [V2 Int] -> StateT (GridCells Cell) m ()
addSpawnPoints spawnPoints =
  forM_ spawnPoints $ \(V2 x y) -> cellAtL x y .= cellOfType SpawnPoint

getPopulation :: MapConfig -> Population
getPopulation MapConfig {extents = (w, h), numTeams, teamSize} =
  let spawnPoints = getSpawnPoints numTeams (V2 w h)
  in Population $ zipWith createTeam [0 ..] spawnPoints
  where
    createTeam teamID spawnPoint =
      AntTeam
      { teamID
      , teamSize
      , client = error "no client defined"
      , ants = []
      , spawnPoints = [spawnPoint]
      , numFood = 0
      }

addFoodRegions :: MonadIO m => Int -> V2 Int -> StateT (GridCells Cell) m ()
addFoodRegions numFoodRegions extents = do
  let offset = 4
      (V2 w h) = extents - fromIntegral offset
      rng = (offset, w)
  centerPoints <-
    liftIO $ randomPoints numFoodRegions ((offset, w), (offset, h))
  forM_ centerPoints $ \(x, y) -> addFoodRegion (V2 x y) (V2 w h)

addFoodRegion :: MonadIO m => V2 Int -> V2 Int -> StateT (GridCells Cell) m ()
addFoodRegion center extents = addRegionRL center extents (cellOfType Food)

addBarrierCells :: MonadIO m => Int -> V2 Int -> StateT (GridCells Cell) m ()
addBarrierCells numBarriers extents@(V2 w h) =
  replicateM_ numBarriers $ addBarrierCellsGroup extents

addBarrierCellsGroup :: MonadIO m => V2 Int -> StateT (GridCells Cell) m ()
addBarrierCellsGroup extents@(V2 w h) = do
  let barrierLength = w `div` 4
      gridCenter = extents `divA` 2
  excenter <- liftIO $ uncurry V2 <$> randomPoint (maxExcenterRng extents)
  d <- liftIO randomDirection
  let direction = V2 d d
      startPos = gridCenter + excenter
  addBarrierCellsGroupLoop 0 barrierLength startPos direction extents

{- |
Iteratively add barrier cells by randomly changing the iteration direction.
-}
addBarrierCellsGroupLoop ::
     MonadIO m
  => Int
  -> Int
  -> V2 Int
  -> V2 Int
  -> V2 Int
  -> StateT (GridCells Cell) m ()
addBarrierCellsGroupLoop i barrierLength pos@(V2 x y) direction@(V2 dx dy) gridExtents
  | i == barrierLength = return ()
  | gridContainsPosition pos gridExtents = do
    cellAtL x y .= cellOfType Barrier
    (dx', dy') <- liftIO $ (,) <$> randomDirection <*> randomDirection
    let dx'' =
          if i `mod` 3 == 0
            then dx'
            else dx
        dy'' =
          if i `mod` 5 == 0
            then dy'
            else dy
        direction' = V2 dx'' dy''
    addBarrierCellsGroupLoop
      (i + 1)
      barrierLength
      (pos + direction')
      direction'
      gridExtents
  | otherwise = do
    d <- liftIO randomDirection
    let direction' = V2 d d
    addBarrierCellsGroupLoop
      (i + 1)
      barrierLength
      (pos + direction')
      direction'
      gridExtents

randomDirection :: IO Int
randomDirection = randomRIO (-1, 1)

gridContainsPosition (V2 x y) (V2 w h) = and [x >= 0, y >= 0, x < w, y < h]

maxExcenterRng :: Integral i => V2 i -> ((i, i), (i, i))
maxExcenterRng extents = ((-x, x), (-y, y))
  where
    (V2 x y) = extents `divA` 4 `divA` 2

makeGridSymmetric :: MonadIO m => StateT (GridCells Cell) m ()
makeGridSymmetric = this %= toSymmetricGrid

{- |
Make grid point symmetric to center.

>>> :{
toSymmetricGrid
  [ [0, 1, 2]
  , [3, 4, 5]
  , [6, 7, 8]
  , [9, 10, 11]
  , [12, 13, 14]
  ]
:}
[[0,1,2],[3,4,9],[6,7,6],[9,4,3],[2,1,0]]

>>> :{
toSymmetricGrid
  [ [0,1,2]
  , [3,4,5]
  , [6,7,8]
  , [9,10,11]
  , [12,13,14]
  , [15,16,17]
  ]
:}
[[0,1,2],[3,4,12],[6,7,9],[9,7,6],[12,4,3],[2,1,0]]

>>> :{
toSymmetricGrid
  [ [ 0,  1,  2,  3,  4]
  , [ 5,  6,  7,  8,  9]
  , [10, 11, 12, 13, 14]
  , [15, 16, 17, 18, 19]
  , [20, 21, 22, 23, 24]
  , [25, 26, 27, 28, 29]
  , [30, 31, 32, 33, 34]
  , [35, 36, 37, 38, 39]
  , [40, 41, 42, 43, 44]
  , [45, 46, 47, 48, 49]
  ]
:}
[[0,1,2,3,4],[5,6,7,8,40],[10,11,12,36,35],[15,16,17,31,30],[20,21,22,26,25],[25,26,22,21,20],[30,31,17,16,15],[35,36,12,11,10],[40,8,7,6,5],[4,3,2,1,0]]

-}
toSymmetricGrid :: GridCells a -> GridCells a
toSymmetricGrid grid =
  let (w, h) = gridExtentsOf grid
      (upper, middle, lower) = splitHalfs grid
      middle' = maybeToList $ reflectVerticalInPlace <$> middle
      upper' =
        zipWith3
          (\i r r' ->
             let m = max (w - i) (w `div` 2 + w `mod` 2)
             in take m r ++ reverse (take (w - m) r'))
          [0 ..]
          upper
          (reverse lower)
  in concat [upper', middle', reverse $ map reverse upper']

{- |
Reflect list at the middle and overwrite the second half of it.

>>> reflectVerticalInPlace []
[]

>>> reflectVerticalInPlace [6]
[6]

>>> reflectVerticalInPlace [6, 7]
[6,6]

>>> reflectVerticalInPlace [6, 7, 8]
[6,7,6]

>>> reflectVerticalInPlace [6, 7, 8, 9]
[6,7,7,6]

>>> reflectVerticalInPlace [5, 6, 7, 8, 9]
[5,6,7,6,5]

-}
reflectVerticalInPlace :: [a] -> [a]
reflectVerticalInPlace [] = []
reflectVerticalInPlace values =
  let (upper, middle, lower) = splitHalfs values
  in concat [upper, maybeToList middle, reverse upper]

{- |
Split a list of values in two halfs: @(firstHalf, middle, secondHalf)@
If the list has even element count then @middle@ is @Nothing@.
Otherwise, @middle@ is @Just@ the element in the middle of the list.

>>> splitHalfs []
([],Nothing,[])

>>> splitHalfs [1,2]
([1],Nothing,[2])

>>> splitHalfs [1]
([],Just 1,[])

>>> splitHalfs [0..6]
([0,1,2],Just 3,[4,5,6])

>>> splitHalfs [0..5]
([0,1,2],Nothing,[3,4,5])

-}
splitHalfs :: [a] -> ([a], Maybe a, [a])
splitHalfs values =
  let l = length values
      l2 = l `div` 2
      middle =
        if even l
          then l2
          else l2 + 1
  in ( take l2 values
     , if even l
         then Nothing
         else Just (values !! l2)
     , drop middle values)

{- |
Generate random points using different ranges for x- and y- coordinate.
-}
randomPoints :: (Random a, Random b) => Int -> ((a, a), (b, b)) -> IO [(a, b)]
randomPoints count bounds = replicateM count (randomPoint bounds)

{- |
Generate random point using different ranges for x- and y- coordinate.
-}
randomPoint :: (Random a, Random b) => ((a, a), (b, b)) -> IO (a, b)
randomPoint (xBounds, yBounds) = (,) <$> randomRIO xBounds <*> randomRIO yBounds

{- |
Length of a vector.

>>> len (V2 3 4)
5.0

-}
len :: Floating a => V2 a -> a
len (V2 x y) = sqrt (x * x + y * y)

{- |
Distance of a position to the center of a grid in percentage.
-}
centerDist :: (Floating f, Integral i) => V2 i -> V2 i -> V2 i -> f
centerDist pos center extents =
  len $
  ((fromIntegral <$> abs (pos - center)) / (fromIntegral <$> (extents `divA` 2))) /
  sqrt 2

{- |
Change each cell of a region depending on the distance to the center and a random value.
-}
addRegionRL ::
     MonadIO m => V2 Int -> V2 Int -> Cell -> StateT (GridCells Cell) m ()
addRegionRL center extents c' = do
  grid <- get
  gen <- liftIO newStdGen
  let topLeft :: V2 Int
      topLeft = center - extents `divA` 2
      region :: Lens' (GridCells a) (GridCells a)
      region =
        areaRL $ intersectRegion (Rectangle (P topLeft) extents) $
        gridRegionOf grid
      changeCell :: (Cell, (Int, Int), Double) -> Cell
      changeCell (c, (ax, ay), rnd) =
        let d = centerDist (V2 ax ay) center extents
        in if (V2 ax ay == center) || (d < 0.8 * rnd)
             then c'
             else c
      randomGrid :: GridCells Double
      randomGrid =
        uncurry gridOfCells (gridExtentsOf grid) $ randomRs (0.0, 1.0) gen
      addGridIndicesAndRandoms ::
           GridCells a -> GridCells (a, (Int, Int), Double)
      addGridIndicesAndRandoms grid =
        zipGridCells3 (,,) grid gridIndicesInf randomGrid
      gr = addGridIndicesAndRandoms grid ^. region . to (map $ map changeCell)
  region .= gr

{- |
Calculate intersection of two regions.

>>> intersectRegion (rect (-4) (-4) 16 16) (rect 0 0 20 20)
Rectangle (P (V2 0 0)) (V2 12 12)

-}
intersectRegion :: Region -> Region -> Region
intersectRegion r1 r2 =
  let (Rectangle (P topLeft1) extents1) = r1
      (Rectangle (P topLeft2) extents2) = r2
      topLeft = max <$> topLeft1 <*> topLeft2
      lowerRight = min <$> (topLeft1 + extents1) <*> (topLeft2 + extents2)
  in Rectangle (P topLeft) (lowerRight - topLeft)

{- |
Get region of a grid.

>>> gridRegionOf $ gridCellNumbers 3 4
Rectangle (P (V2 0 0)) (V2 3 4)

-}
gridRegionOf :: GridCells a -> Region
gridRegionOf rows = Rectangle (P (V2 0 0)) $ uncurry V2 $ gridExtentsOf rows

{- |
Get extents of a grid.

>>> gridExtentsOf $ gridCellNumbers 3 4
(3,4)

-}
gridExtentsOf :: GridCells a -> Extents
gridExtentsOf rows =
  case rows of
    r:rs -> (length r, length rows)
    _ -> (0, length rows)

{- |
Show mapped value but set without mapping.

>>> 1 & showMapped succ %~ (* 3)
6

>>> [0..3] & mapped . showMapped succ %~ (* 3)
[3,6,9,12]

-}
showMapped :: (s -> a) -> Lens s t a t
showMapped f = lens f $ flip const

{- |
Create grid from list of cells.

>>> gridOfCells 2 3 [0..]
[[0,1],[2,3],[4,5]]

-}
gridOfCells :: Int -> Int -> [c] -> GridCells c
gridOfCells w h cells = take h $ chunksOf w cells

{- |
Grid of cell numbers as cells

>>> gridCellNumbers 4 3
[[0,1,2,3],[4,5,6,7],[8,9,10,11]]

-}
gridCellNumbers :: (Num e, Enum e) => Int -> Int -> [[e]]
gridCellNumbers w h = gridOfCells w h [0 ..]

{- |
Grid of indeces as cells

>>> gridIndices 2 3
[[(0,0),(1,0)],[(0,1),(1,1)],[(0,2),(1,2)]]

-}
gridIndices :: (Num t, Num t1, Enum t, Enum t1) => t1 -> t -> [[(t1, t)]]
gridIndices w h = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

{- |
Infinite version of @gridIndices@.

>>> addGridIndices $ gridCellNumbers 2 3
[[(0,(0,0)),(1,(1,0))],[(2,(0,1)),(3,(1,1))],[(4,(0,2)),(5,(1,2))]]

-}
gridIndicesInf :: [[(Int, Int)]]
gridIndicesInf = [[(x, y) | x <- [0 ..]] | y <- [0 ..]]

{- |
Add indices to grid cells.

>>> addGridIndices [[0..2],[3..5]]
[[(0,(0,0)),(1,(1,0)),(2,(2,0))],[(3,(0,1)),(4,(1,1)),(5,(2,1))]]

-}
addGridIndices :: GridCells a -> GridCells (a, (Int, Int))
addGridIndices g = zipGridCells (,) g gridIndicesInf

{- |
Access cells in an area of the grid.

>>> gridIndices 10 10 ^. areaCells 3 1 4 2
[(3,1),(4,1),(5,1),(6,1),(3,2),(4,2),(5,2),(6,2)]

-}
areaCells ::
     Applicative f
  => Int
  -> Int
  -> Int
  -> Int
  -> ([a] -> f [a])
  -> [[a]]
  -> f [[a]]
areaCells x y w h = dropL y . takeL h . traverse . dropL x . takeL w

{- |
Access cells in an area of the grid.

>>> gridIndices 10 10 ^. areaL 3 1 4 2
[[(3,1),(4,1),(5,1),(6,1)],[(3,2),(4,2),(5,2),(6,2)]]

-}
areaL :: Int -> Int -> Int -> Int -> Lens' (GridCells a) (GridCells a)
areaL x y w h = lens (area x y w h) (\s a -> replaceArea x y w h a s)

{- |
Lens version of @areaL@.
-}
areaRL :: Rectangle Int -> Lens' (GridCells a) (GridCells a)
areaRL (Rectangle (P (V2 x y)) (V2 w h)) = areaL x y w h

{- |
Access cells in an area of the grid.

>>> area 3 1 4 2 $ gridIndices 10 10
[[(3,1),(4,1),(5,1),(6,1)],[(3,2),(4,2),(5,2),(6,2)]]

-}
area :: Int -> Int -> Int -> Int -> GridCells a -> GridCells a
area x y w h g = map (subSeq x w) $ subSeq y h g

{- |
Combine a getter lens with a setter lens.
-}
mapL :: Getting a s a -> ASetter s t a' b -> Lens s t a b
mapL getL setL = lens (\s -> s ^. getL) (\s v -> s & setL .~ v)

{- |
Map a sub-sequence of a list.

>>> mapSubSeq (map (* 2)) 3 4 [0..9]
[0,1,2,6,8,10,12,7,8,9]

>>> mapSubSeq (drop 2) 3 4 [0..9]
[0,1,2,5,6,7,8,9]

-}
mapSubSeq :: ([a] -> [a]) -> Int -> Int -> [a] -> [a]
mapSubSeq f i length values =
  let h = take i values
      m = subSeq i length values
      t = drop (i + length) values
  in h ++ f m ++ t

{- |
Get a sub-sequence of a list.

>>> subSeq 3 4 [0..9]
[3,4,5,6]

-}
subSeq :: Int -> Int -> [a] -> [a]
subSeq i length values = take length $ drop i values

{- |
Lens version of @subSeq@.

Multiply the 4 values after the first 3 values with 2.
>>> [0..9] & subSeqL 3 4 %~ map (*2)
[0,1,2,6,8,10,12,7,8,9]

Remove 4 values after the first 3 values.
>>> [0..9] & subSeqL 3 4 .~ []
[0,1,2,7,8,9]

-}
subSeqL :: Int -> Int -> Lens' [a] [a]
subSeqL i length = dropL i . takeL length

{- |
Map each cell of an area in a grid using a mapping function.
-}
mapAreaCells ::
     Int -> Int -> Int -> Int -> (a -> a) -> GridCells a -> GridCells a
mapAreaCells x y w h f g = mapSubSeq (map $ mapSubSeq (map f) x w) y h g

{- |
Replace an area of a grid with another grid.

>>> replaceArea 3 1 4 2 (gridCellNumbers 2 2) (gridCellNumbers 5 5)
[[0,1,2,3,4],[5,6,7,0,1],[10,11,12,2,3],[15,16,17,18,19],[20,21,22,23,24]]

-}
replaceArea ::
     Int -> Int -> Int -> Int -> GridCells a -> GridCells a -> GridCells a
replaceArea x y w h g' g =
  mapSubSeq (zipWith (\row' row -> mapSubSeq (\_ -> row') x w row) g') y h g

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

>>> [[0..2],[3..5]] ^? cellAtL 2 1
Just 5

>>> [[0..2],[3..5]] & cellAtL 2 1 %~ (* 2)
[[0,1,2],[3,4,10]]

-}
cellAtL ::
     (Ixed (IxValue m), Ixed m, Applicative f)
  => Index (IxValue m)
  -> Index m
  -> (IxValue (IxValue m) -> f (IxValue (IxValue m)))
  -> m
  -> f m
cellAtL x y = ix y . ix x

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
Zip to grids of cells using a mapping function.

>>> :{
zipGridCells3
    (\n1 text n2 -> show n1 ++ [text] ++ show n2)
    [[0,1], [2,3]]
    [['a','b'], ['c','d']]
    [[4,5], [6,7]]
:}
[["0a4","1b5"],["2c6","3d7"]]

-}
zipGridCells3 ::
     (a -> b -> c -> d)
  -> GridCells a
  -> GridCells b
  -> GridCells c
  -> GridCells d
zipGridCells3 fn = zipWith3 (zipWith3 fn)

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