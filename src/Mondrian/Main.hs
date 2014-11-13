module Main where

import Mondrian
import Export

main :: IO ()
main = do
  painting <- randPainting 1920 1200
  crop 1920 1200 (insertGrid painting) `toFile` "Oulala.png"
