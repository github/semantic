module Reprinting.Typeset (typeset) where

import Prologue

import Data.Text.Prettyprint.Doc
import Reprinting.Translate

typeset :: Seq Splice -> Doc a
typeset = foldMap go where
  go (Insert t)             = pretty t
  go (Directive Soft)       = softline
  go (Directive (Hard i t)) = line <> stimes i (space t)
  go (Directive Don't)      = mempty
  space Space = " "
  space Tab   = "\t"
