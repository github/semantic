module Parser where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified Data.Set as Set

type Parser = String -> IO (Term String Info)

lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl annotateLeaves ([], 0, 0) lines of
  (leaves, _, _) -> leaves
  where
    lines = Prelude.lines input
    root syntax = Info (Range 0 $ length input) (Range 0 $ length lines) Set.empty :< syntax
    leaf charIndex lineIndex line = Info (Range charIndex $ charIndex + length line) (Range lineIndex $ lineIndex + 1) Set.empty :< Leaf line
    annotateLeaves (accum, charIndex, lineIndex) line =
      (accum ++ [ leaf charIndex lineIndex line ]
      , charIndex + length line + 1
      , lineIndex + 1)
