module Parsers where

import Diff
import Range
import Parser
import Syntax
import Term
import TreeSitter
import Control.Comonad.Cofree
import qualified Data.Text as T

parserForType :: T.Text -> Parser
parserForType mediaType = maybe lineByLineParser parseTreeSitterFile $ languageForType mediaType

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = T.lines input
    root syntax = Info (Range 0 $ T.length input) mempty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + T.length line) mempty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex line ]
      , charIndex + T.length line + 1)
