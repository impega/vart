module Main where

import Mondrian
import Export

main :: IO ()
main = do
  let painting =
       flip (foldr cutHorizontal) [40, 100, 170, 240, 330, 390] $
       flip (foldr cutVertical)   [60, 170, 230, 280, 310, 490] $ blankCanvas 400 600
  repaint <- fmap Painting $ randColors $ grid painting
  insertGrid repaint `toFile` "Oulala.png"
