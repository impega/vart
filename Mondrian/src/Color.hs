{-# LANGUAGE MultiWayIf #-}

module Color where

import Codec.Picture.Types
import Control.Monad.Random

data Color = White | Blue | Yellow | Red

randColor :: MonadRandom m => m Color
randColor = do
  r <- getRandomR (0, 200 :: Int)
  return $
    if | r <= 125  -> White
       | r <= 150  -> Blue
       | r <= 175  -> Yellow
       | otherwise -> Red

toRGB8 :: Maybe Color -> PixelRGB8
toRGB8 (Just c) = case c of
  White  -> PixelRGB8 255 255 255
  Blue   -> PixelRGB8 34 80 149
  Yellow -> PixelRGB8 250 201 1
  Red    -> PixelRGB8 221 1 0
toRGB8 Nothing = PixelRGB8 0 0 0
