module Mondrian where

import Prelude hiding (foldr)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable

import System.Random

data NeList a = Nil a | Cons a (NeList a)

instance Functor NeList where
  fmap f (Nil x)     = Nil $ f x
  fmap f (Cons x xs) = f x `Cons` fmap f xs

append :: NeList a -> NeList a -> NeList a
append (Nil x)     ys = Cons x ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Monad NeList where
  return      = Nil
  Nil x     >>= f = f x
  Cons x xs >>= f = append (f x) $ xs >>= f

newtype Painting = Painting { grid :: NeList (Int, Column) }
type Column      = NeList (Int, Either Color Painting)
data Color       = White | Blue | Yellow | Red

cutNeList :: Int -> NeList (Int, a) -> NeList (Int, a)
cutNeList m xs@(Nil (n, a))
  | m <  n = Cons (m, a) $ Nil (n - m, a)
  | m == n = xs
  | n < m  = Cons (n, a) $ Nil (m - n, a)
cutNeList m xs@(Cons (n, a) cols)
  | m <  n = Cons (m, a) $ Cons (n - m, a) cols
  | m == n = xs
  | n < m  = Cons (n, a) $ cutNeList (m - n) cols

cutVertical :: Int -> Painting -> Painting
cutVertical m = Painting . cutNeList m . grid

cutHorizontal :: Int -> Painting -> Painting
cutHorizontal m = Painting . fmap (fmap $ cutNeList m) . grid

blankCanvas :: Int -> Int -> Painting
blankCanvas width height = Painting $ Nil (width, Nil (height, Left White))

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

instance Foldable NeList where
  foldr cons nil (Nil x)     = cons x nil
  foldr cons nil (Cons x xs) = cons x $ foldr cons nil xs


instance Traversable NeList where
  traverse f (Nil x)     = fmap Nil $ f x
  traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

randColorsColumn :: NeList (Int, Either Color Painting) -> IO (NeList (Int, Either Color Painting))
randColorsColumn = traverse (\ (h, _) -> randColor >>= \ c -> return (h, Left c))

randColors :: NeList (Int, Column) -> IO (NeList (Int, Column))
randColors = traverse (\ (w, cols) -> randColorsColumn cols >>= \ cols' -> return (w, cols'))

main :: IO ()
main = do
  let painting =
       flip (foldr cutHorizontal) [2, 6, 17, 24] $
       flip (foldr cutVertical)   [1, 5, 10, 12] $ blankCanvas 20 30
  repaint <- fmap Painting $ randColors $ grid painting
  putStrLn $ showConsol repaint

