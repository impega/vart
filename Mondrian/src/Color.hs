module Color where

import Codec.Picture.Types
import Control.Monad.Random

data Color = White | Blue | Yellow | Red

randColor :: MonadRandom m => m Color
randColor = do
  r <- getRandomR (0, 2.0 :: Float)
  return $
    if      r <= 1.7 then White
    else if r <= 1.8 then Blue
    else if r <= 1.9 then Yellow
    else Red

toRGB8 :: Maybe Color -> PixelRGB8
toRGB8 (Just c) = case c of
  White  -> PixelRGB8 255 255 255
  Blue   -> PixelRGB8 0 0 255
  Yellow -> PixelRGB8 255 255 0
  Red    -> PixelRGB8 255 0 0
toRGB8 Nothing = PixelRGB8 0 0 0
