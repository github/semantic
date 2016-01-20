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
import Term

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
      , charIndex + length line + 1)
    toText = T.pack . Source.toString

-- | Replace every string leaf with leaves of the words in the string.
breakDownLeavesByWord :: Source Char -> Term T.Text Info -> Term T.Text Info
breakDownLeavesByWord source = cata replaceIn
  where
    replaceIn info@(Info range categories) (Leaf _) | ranges <- rangesAndWordsInSource range, length ranges > 1 = info :< (Indexed $ makeLeaf categories <$> ranges)
    replaceIn info syntax = info :< syntax
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (Source.toList $ slice range source)
    makeLeaf categories (range, substring) = Info range categories :< Leaf (T.pack substring)

