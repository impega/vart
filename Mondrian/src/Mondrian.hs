{-# LANGUAGE TupleSections #-}

module Mondrian where

import Prelude              hiding (mapM)
import Control.Monad        hiding (mapM)
import Control.Monad.Random
import Data.List
import Data.Traversable

-- Modelling a painting as a list of lists. The cell may either be
newtype Painting = Painting { grid :: [(Int, Column)] }
type Column      = [(Int, Cell)]
type Cell        = Either Color Painting
data Color       = White | Blue | Yellow | Red | Black

-- We may introduce a new cut in the painting. If the cut is after the
-- end of the painting, the impact is null.
cutAt :: Int -> [(Int, a)] -> [(Int, a)]
cutAt _ [] = []
cutAt m xs@((n, a) : cols)
  | m <  n = (m, a) : (n - m, a) : cols
  | m == n = xs
  | n < m  = (n, a) : cutAt (m - n) cols

cutVertical :: Int -> Painting -> Painting
cutVertical m = Painting . cutAt m . grid

cutHorizontal :: Int -> Painting -> Painting
cutHorizontal m = Painting . fmap (fmap $ cutAt m) . grid

cropAt :: Int -> [(Int, a)] -> [(Int, a)]
cropAt len [] = []
cropAt len ((n, a) : tl)
  | n <= len  = (n, a) : cropAt (len - n) tl
  | len == 0  = []
  | otherwise = [(len, a)]

crop :: Int -> Int -> Painting -> Painting
crop w h = Painting . fmap (uncurry cropCol) . cropAt w . grid
  where
    cropCol :: Int -> Column -> (Int, Column)
    cropCol w' = (w',) . fmap (uncurry $ cropCell w') . cropAt h
    cropCell :: Int -> Int -> Cell -> (Int, Cell)
    cropCell w' h' = (h',) . fmap (crop w' h')

-- We may introduce a grid by adding Black edges between cells
insertGrid :: Painting -> Painting
insertGrid (Painting canvas) =
  let
  -- we start by drawing grids [r]ecursively and [h]orizontally
      rhgrid  =
        fmap (fmap $ \ cols ->
             let rgrid = fmap (fmap $ fmap insertGrid) cols
             in intersperse (5, Left Black) rgrid)
        canvas
  -- finally we draw the [v]ertical ones
      nheight = sum $ map fst . snd . head $ rhgrid
      rhvgrid = intersperse (5, [(nheight, Left Black)]) rhgrid
  in Painting rhvgrid

-- A blank canvas to start from.
blankCanvas :: Int -> Int -> Painting
blankCanvas width height = Painting $ [(width, [(height, Left White)])]

randMondrian :: MonadRandom m => Int -> Int -> m Painting
randMondrian width height = do
  painting <- randPainting width height
  return $ crop width height $ insertGrid painting

randPainting :: MonadRandom m => Int -> Int -> m Painting
randPainting w h = do
  -- pick a number of cuts to make
  nw <- getRandomR (0 , max 2 $ w `div` 70)
  nh <- getRandomR (0 , max 2 $ h `div` 70)
  -- vertical & horizontal cuts
  vc <- liftM (take nw) $ getRandomRs (0, w)
  hc <- liftM (take nh) $ getRandomRs (0, h)
  -- performing the cuts & filling in the cells
  randCells
    $ flip (foldr cutHorizontal) hc
    $ flip (foldr cutVertical) vc
    $ blankCanvas w h

randCell :: MonadRandom m => Int -> Int -> m Cell
randCell w h = do
  r <- getRandomR (0, 1.0 :: Float)
  if 10 < h - w && r <= 0.2
  then liftM Right $ randPainting w h
  else liftM Left  $ randColor

randColor :: MonadRandom m => m Color
randColor = do
  r <- getRandomR (0, 2.0 :: Float)
  return $
    if      r <= 1.7 then White
    else if r <= 1.8 then Blue
    else if r <= 1.9 then Yellow
    else Red

randCells :: MonadRandom m => Painting -> m Painting
randCells = liftM Painting . mapM (uncurry col) . grid
  where
    col  w   = liftM (w,) . mapM (uncurry $ cell w)
    cell w h = liftM (h,) . const (randCell w h)

-- Quick and dirty display function to print the first level
-- paintings in the console using escape codes.
showColorConsol :: Color -> String
showColorConsol White  = "\x1B[107m \x1B[0m"
showColorConsol Blue   = "\x1B[44m \x1B[0m"
showColorConsol Yellow = "\x1B[43m \x1B[0m"
showColorConsol Red    = "\x1B[41m \x1B[0m"

showPaintingConsol :: Painting -> String
showPaintingConsol (Painting xs) = foldr ((++) . showCol) "" xs
  where
    showBox (h, zs) = join $ replicate h $ either showColorConsol showPaintingConsol zs
    showCol (w, ys) = join $ replicate w $ foldr ((++) . showBox) "" ys ++ "\n"


