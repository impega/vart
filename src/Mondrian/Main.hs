module Main where

import Mondrian
import Export

main :: IO ()
main = do
  painting <- randPainting 700 900
  insertGrid painting `toFile` "Oulala.png"
