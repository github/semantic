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
step (Unhandled _ _)      = mempty -- TODO: Do we drop uhandled splices at this point?
step (Original t  )       = pretty t
step (Insert _ _ t)       = pretty t
step (Directive SoftWrap) = softline
step (Directive HardWrap) = line
step (Directive Space)    = space
step (Directive Indent)   = stimes (2 :: Int) space -- TODO: Configuration of tabs v. spaces
