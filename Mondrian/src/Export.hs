module Export where

import Color
import Mondrian
import Codec.Picture
import Codec.Picture.Types

import Control.Monad
import Control.Monad.Primitive

type Drawing m = MutableImage (PrimState m) PixelRGB8

drawPaintingAt :: PrimMonad m => Int -> Int -> Painting -> Drawing m -> m ()
drawPaintingAt x y (Painting canvas) img =
  foldM_ (\ x' (w, cols) ->
         foldM_ (\ y' (h, cell) ->
                drawCellAt x' y' (w, h, cell) img
                >> return (y' + h))
           y cols
         >> return (x' + w))
    x canvas

drawCellAt :: PrimMonad m => Int -> Int -> (Int, Int, Cell) -> Drawing m -> m ()
drawCellAt x y (_, _, Right painting) img = drawPaintingAt x y painting img
drawCellAt x y (w, h, Left color)     img =
  let pixel = toRGB8 color in
  forM_ [x..x+w-1] $ do \ i -> forM_ [y..y+h-1] $ do \ j -> writePixel img i j pixel

toImage :: PrimMonad m => Painting -> m (Image PixelRGB8)
toImage painting = do
  let canvas = grid painting
  let width  = sum $ map fst canvas
  let height = sum $ map fst $ snd $ head canvas
  image <- createMutableImage width height $ toRGB8 (Just White)
  () <- drawPaintingAt 0 0 painting image
  unsafeFreezeImage image

toFile :: Painting -> FilePath -> IO ()
toFile painting fp = toImage painting >>= savePngImage fp . ImageRGB8
