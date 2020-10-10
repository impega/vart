{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE DeriveFunctor   #-}

module Mondrian2 where

import Control.Monad.Random
import Color

data Painting' a = Painting
  { width  :: !Int
  , height :: !Int
  , canvas :: a
  } deriving (Functor)

type Frame    = Painting' ()
type Painting = Painting' Canvas

data Canvas
  = VCut Painting !Int Painting -- Left, border, right
  | HCut Painting !Int Painting -- Top, border, bottom
  | Solid Color                 -- Base case

solid :: MonadRandom m => Frame -> m Painting
solid p = do
  c <- randColor
  pure $ Solid c <$ p

painting :: MonadRandom m => Frame -> m Painting
painting p
  | width p <= 200 || height p <= 200 = solid p
  | (width p >= 500 && height p >= 60) ||
    (width p >= 60 && height p >= 500) = do
    r <- getRandomR (0, 20 :: Int)
    if | r <= 10  -> vcut p
       | otherwise -> hcut p
  | otherwise = do
    r <- getRandomR (0, 25 :: Int)
    if | r <= 10  -> vcut p
       | r <= 20  -> hcut p
       | otherwise -> solid p

vcut :: MonadRandom m => Frame -> m Painting
vcut p = cut VCut (width p) (\ i -> p { width = i })

hcut :: MonadRandom m => Frame -> m Painting
hcut p = cut HCut (height p) (\ i -> p { height = i })

cut :: MonadRandom m
    => (Painting -> Int -> Painting -> Canvas) -- How to pack the pieces
    -- Ideally the two following arguments would be replaced by a lens and a frame
    -> Int            -- Dimension to cut
    -> (Int -> Frame) -- Frame (held by the side to cut)
    -> m Painting
cut c total frame = do
  border <- getRandomR (5, 20 :: Int)
  size1 <- getRandomR (20, total - border - 20)
  let size2 = total - border - size1
  part1 <- painting (frame size1)
  part2 <- painting (frame size2)
  pure $ c part1 border part2 <$ frame total

randMondrian :: MonadRandom m => Int -> Int -> m Painting
randMondrian width height = painting (Painting width height ())
