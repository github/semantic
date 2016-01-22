module Parsers where

import Diff
import Range
import Parser
import Source hiding ((++))
import Syntax
import TreeSitter
import Control.Comonad.Cofree
import qualified Data.Text as T
import Data.Foldable

parserForType :: T.Text -> Parser
parserForType mediaType = maybe lineByLineParser parseTreeSitterFile $ languageForType mediaType

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = actualLines input
    root syntax = Info (Range 0 $ length input) mempty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + T.length line) mempty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex (toText line) ]
      , charIndex + length line)
    toText = T.pack . Source.toString
