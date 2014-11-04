module Mondrian where

import Control.Monad
import Data.List
import Data.Traversable
import System.Random

-- Modelling a painting as a list of lists. The cell may either be
newtype Painting = Painting { grid :: [(Int, Column)] }
type Column      = [(Int, Either Color Painting)]
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

-- We may introduce a grid by adding Black edges between
-- cells
insertGrid :: Painting -> Painting
insertGrid (Painting canvas) =
  let rhgrid  = fmap (fmap $ \ cols ->
                 -- first we start by drawing a grid [r]ecursively on subpaintings
                 let rgrid = fmap (fmap (either Left (Right . insertGrid))) cols
                 -- then we insert [h]orizontal lines
                 in intersperse (5, Left Black) rgrid) canvas
      -- finally we draw the [v]ertical ones
      height  = sum $ map fst . snd . head $ rhgrid
      rhvgrid = intersperse (5, [(height, Left Black)]) rhgrid
  in Painting rhvgrid

-- A blank canvas to start from.
blankCanvas :: Int -> Int -> Painting
blankCanvas width height = Painting $ [(width, [(height, Left White)])]

randColor :: IO Color
randColor = do
  r <- randomRIO (0, 2.0 :: Float)
  return $
    if      r <= 1.7 then White
    else if r <= 1.8 then Blue
    else if r <= 1.9 then Yellow
    else Red

randColorsColumn :: [(Int, Either Color Painting)] -> IO [(Int, Either Color Painting)]
randColorsColumn = traverse (traverse $ either dealWithColor dealWithRec)
  where dealWithColor = const $ fmap Left randColor
        dealWithRec   = fmap (Right . Painting) . randColors . grid

randColors :: [(Int, Column)] -> IO [(Int, Column)]
randColors = traverse $ traverse randColorsColumn


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


