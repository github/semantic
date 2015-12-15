module Parser where

import Diff
import Range
import Syntax
import Term
import TreeSitter
import Control.Comonad.Cofree
import qualified Data.Set as Set

type Parser = String -> IO (Term String Info)

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = Prelude.lines input
    root syntax = Info (Range 0 $ length input) Set.empty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + length line) Set.empty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex line ]
      , charIndex + length line + 1)
