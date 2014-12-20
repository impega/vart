module Main where

import           Data.Text (Text)
import           Data.List
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Control.Monad
import System.Console.ArgParser

newtype Needle   = Needle   { outNeedle   :: Text   }
newtype Haystack = Haystack { outHaystack :: [Text] }
newtype Sentence = Sentence { outSentence :: [Text] }

cleanUp :: Text -> Text
cleanUp = T.toLower . T.filter (`notElem` ",-.?!:")

nextOccurence :: Needle -> Haystack -> Maybe Haystack
nextOccurence (Needle n) (Haystack h) =
  let next = dropWhile ((n /=) . cleanUp) h in
  if null next
    then Nothing
    else Just $ Haystack $ tail next

nextSentence :: Int -> Sentence -> Haystack -> Maybe (Sentence, (Sentence, Haystack))
nextSentence size (Sentence s) hc =
  let needle = Needle $ cleanUp $ last s in
  do
    Haystack hn <- nextOccurence needle hc
    guard (length hn > size)
    let (sn, hr) = splitAt size hn
    let sentence = Sentence sn
    return $ (sentence, (sentence, Haystack hr))

exquisiteCorpse :: Int -> Sentence -> Haystack -> [Sentence]
exquisiteCorpse n = curry $ unfoldr (uncurry $ nextSentence n)


data Config =
  Config { size   :: Int
         , seed   :: String
         , source :: String }

configParser :: ParserSpec Config
configParser =
  Config
  `parsedBy` reqPos "size"     `Descr` "size of the chunks"
  `andBy`    reqPos "seed"     `Descr` "seed sentence"
  `andBy`    reqPos "filepath" `Descr` "filepath to the source text"

main :: IO ()
main = withParseResult configParser $ \ config -> do
  alice <- TIO.readFile $ source config
  let first  = Sentence $ T.words $ T.pack $ seed config
  let corpse = exquisiteCorpse (size config) first $ Haystack $ T.words alice
  mapM_ TIO.putStrLn $ (T.pack $ seed config) : fmap (T.unwords . outSentence) corpse

