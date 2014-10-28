module Export where

import Mondrian
import Codec.Picture
import Codec.Picture.Types

import Control.Monad
import Control.Monad.Primitive

type Drawing m = MutableImage (PrimState m) PixelRGB8

toRGB8 :: Color -> PixelRGB8
toRGB8 White  = PixelRGB8 255 255 255
toRGB8 Blue   = PixelRGB8 0 0 255
toRGB8 Yellow = PixelRGB8 255 255 0
toRGB8 Red    = PixelRGB8 255 0 0
toRGB8 Black  = PixelRGB8 0 0 0

drawPaintingAt :: PrimMonad m => Int -> Int -> Painting -> Drawing m -> m ()
drawPaintingAt x y (Painting canvas) img =
  foldM_ (\ x' (w, cols) ->
         foldM_ (\ y' (h, cell) ->
                drawCellAt x' y' (w, h, cell) img
                >> return (y' + 10 * h))
           y cols
         >> return (x' + 10 * w))
    x canvas

drawCellAt :: PrimMonad m => Int -> Int -> (Int, Int, Either Color Painting) -> Drawing m -> m ()
drawCellAt x y (_, _, Right painting) img = drawPaintingAt x y painting img
drawCellAt x y (w, h, Left color)     img =
  let pixel = toRGB8 color in
  forM_ [x..x+10*w-1] $ do \ i -> forM_ [y..y+10*h-1] $ do \ j -> writePixel img i j pixel

toImage :: PrimMonad m => Painting -> m (Image PixelRGB8)
toImage painting = do
  let canvas = grid painting
  let width  = sum $ map fst canvas
  let height = sum $ map fst $ snd $ head canvas
  image <- createMutableImage (10 * width) (10 * height) $ toRGB8 White
  () <- drawPaintingAt 0 0 painting image
  freezeImage image

toFile :: Painting -> FilePath -> IO ()
toFile painting fp = toImage painting >>= savePngImage fp . ImageRGB8


