module Semantic.Util where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

pp :: Pretty a => a -> IO ()
pp = putDocW 100 . (<> line) . pretty
