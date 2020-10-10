module Main where

import Control.Applicative
import Options
import Mondrian2
import Export2

data MondrianOptions = MondrianOptions
  { optWidth    :: Int
  , optHeight   :: Int
  , optFilepath :: FilePath
  }

shortSimpleOption
  :: SimpleOptionType a
  => Char   -- short flag
  -> a      -- default value
  -> String -- description
  -> DefineOptions a
shortSimpleOption flag def desc =
  defineOption simpleOptionType (\o -> o
        { optionShortFlags = [flag]
        , optionDefault = def
        , optionDescription = desc
        })

instance Options MondrianOptions where
  defineOptions =
    MondrianOptions
    <$> shortSimpleOption 'w' 1920       "Width of the generated image"
    <*> shortSimpleOption 'h' 1200       "Height of the generated image"
    <*> shortSimpleOption 'o' "Mondrian" "Filepath of the generated image"

main :: IO ()
main = runCommand $ \ opts args -> do
  painting <- randMondrian (optWidth opts) (optHeight opts)
  toFile painting $ optFilepath opts ++ ".png"
