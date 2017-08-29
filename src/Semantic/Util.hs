module Semantic.Util where

import Data.Blob
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import Files
import Language

pp :: Pretty a => a -> IO ()
pp = putDocW 100 . (<> line) . pretty

file :: FilePath -> IO Blob
file path = Files.readFile path (languageForType path)
