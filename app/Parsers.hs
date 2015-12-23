module Parsers where

import Diff
import Range
import Parser
import Syntax
import TreeSitter
import Control.Comonad.Cofree

parserForType :: String -> Parser
parserForType mediaType = maybe lineByLineParser parseTreeSitterFile $ languageForType mediaType

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = Prelude.lines input
    root syntax = Info (Range 0 $ length input) mempty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + length line) mempty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex line ]
      , charIndex + length line + 1)
