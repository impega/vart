module Mondrian where

import Control.Monad
import Data.Traversable
import System.Random

newtype Painting = Painting { grid :: [(Int, Column)] }
type Column      = [(Int, Either Color Painting)]
data Color       = White | Blue | Yellow | Red

-- Beware: cutAt is a partial function!
cutAt :: Int -> [(Int, a)] -> [(Int, a)]
cutAt m xs@((n, a) : cols)
  | m <  n = (m, a) : (n - m, a) : cols
  | m == n = xs
  | n < m  = (n, a) : cutAt (m - n) cols

cutVertical :: Int -> Painting -> Painting
cutVertical m = Painting . cutAt m . grid

cutHorizontal :: Int -> Painting -> Painting
cutHorizontal m = Painting . fmap (fmap $ cutAt m) . grid

blankCanvas :: Int -> Int -> Painting
blankCanvas width height = Painting $ [(width, [(height, Left White)])]

instance Show Color where
  show White  = "\x1B[107m \x1B[0m"
  show Blue   = "\x1B[44m \x1B[0m"
  show Yellow = "\x1B[43m \x1B[0m"
  show Red    = "\x1B[41m \x1B[0m"

showConsol :: Painting -> String
showConsol (Painting xs) = foldr ((++) . showCol) "" xs
  where
    showBox (h, zs) = join $ replicate h $ either show showConsol zs
    showCol (w, ys) = join $ replicate w $ foldr ((++) . showBox) "" ys ++ "\n"

randColor :: IO Color
randColor = do
  r <- randomRIO (0, 2.0 :: Float)
  return $
    if      r <= 1.7 then White
    else if r <= 1.8 then Blue
    else if r <= 1.9 then Yellow
    else Red

randColorsColumn :: [(Int, Either Color Painting)] -> IO [(Int, Either Color Painting)]
randColorsColumn = traverse (\ (h, _) -> randColor >>= \ c -> return (h, Left c))

randColors :: [(Int, Column)] -> IO [(Int, Column)]
randColors = traverse (\ (w, cols) -> randColorsColumn cols >>= \ cols' -> return (w, cols'))

main :: IO ()
main = do
  let painting =
       flip (foldr cutHorizontal) [2, 6, 17, 24] $
       flip (foldr cutVertical)   [1, 5, 10, 12] $ blankCanvas 20 30
  repaint <- fmap Painting $ randColors $ grid painting
  putStrLn $ showConsol repaint

