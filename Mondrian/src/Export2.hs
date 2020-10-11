module Export2 where

import Color
import Mondrian2
import Codec.Picture
import Codec.Picture.Types

import Control.Monad
import Control.Monad.Primitive

type Drawing m = MutableImage (PrimState m) PixelRGB8

drawSolidAt :: PrimMonad m => Int -> Int -> Painting' (Maybe Color) -> Drawing m -> m ()
drawSolidAt x y p img = do
  let pixel = toRGB8 (canvas p)
  let xrange = x + width p - 1
  let yrange = y + height p - 1
  forM_ [x..xrange] $ \ i ->
    forM_ [y..yrange] $ \ j ->
      writePixel img i j pixel

borderColor :: Border -> Maybe Color
borderColor b = White <$ guard (visible b)

drawPaintingAt :: PrimMonad m => Int -> Int -> Painting -> Drawing m -> m ()
drawPaintingAt x y p i = case canvas p of
  Solid c    -> drawSolidAt x y (Just c <$ p) i
  VCut l m r -> do
    drawPaintingAt x y l i
    drawSolidAt (x + width l) y (Painting (size m) (height p) (borderColor m)) i
    drawPaintingAt (x + width l + size m) y r i
  HCut t m b -> do
    drawPaintingAt x y t i
    drawSolidAt x (y + height t) (Painting (width p) (size m) (borderColor m)) i
    drawPaintingAt x (y + height t + size m) b i

toImage :: PrimMonad m => Painting -> m (Image PixelRGB8)
toImage painting = do
  image <- createMutableImage (width painting) (height painting) $ toRGB8 (Just White)
  () <- drawPaintingAt 0 0 painting image
  unsafeFreezeImage image

toFile :: Painting -> FilePath -> IO ()
toFile painting fp = toImage painting >>= savePngImage fp . ImageRGB8
