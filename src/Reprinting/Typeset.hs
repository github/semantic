module Reprinting.Typeset
  ( typeset
  , typesetting
  ) where

import Prologue

import Data.Machine
import Data.Reprinting.Splice
import Data.Text.Prettyprint.Doc

typeset :: Seq Splice -> Doc a
typeset = foldMap step

typesetting :: Monad m => ProcessT m Splice (Doc a)
typesetting = auto step

step :: Splice -> Doc a
step (Directive Don't)          = mempty
step (Original t  )             = pretty t
step (Insert _ _ t)             = pretty t
step (Directive SoftWrap)       = softline
step (Directive (HardWrap 0 _)) = line
step (Directive (HardWrap i t)) = line <> stimes i (space t)
  where
    space Space = " "
    space Tab   = "\t"
