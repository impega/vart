module Main where

import Mondrian
import Export

main :: IO ()
main = do
  let painting =
       flip (foldr cutHorizontal) [2, 6, 8, 17, 24, 28] $
       flip (foldr cutVertical)   [1, 5, 10, 12, 15, 19] $ blankCanvas 20 30
  repaint <- fmap Painting $ randColors $ grid painting
  repaint `toFile` "Oulala.png"
