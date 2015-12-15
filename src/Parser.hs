module Parser where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified Data.Set as Set
import qualified Data.Text as T

type Parser = T.Text -> IO (Term T.Text Info)

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines :: [T.Text]
    lines = T.lines input
    root syntax = Info (Range 0 $ T.length input) Set.empty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + T.length line) Set.empty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex line ]
      , charIndex + T.length line + 1)
