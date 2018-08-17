module Reprinting.Typeset
  ( typeset
  , typeSettingRule
  ) where

import Prologue

import Control.Rule
import Data.Source
import Data.Text.Prettyprint.Doc
import Reprinting.Translate

typeset :: Seq Splice -> Doc a
typeset = foldMap go where
  go (Insert t)                 = pretty t
  go (Directive SoftWrap)       = softline
  go (Directive (HardWrap 0 _)) = line
  go (Directive (HardWrap i t)) = line <> stimes i (space t)
  go (Directive Don't)          = mempty
  space Space = " "
  space Tab   = "\t"

typeSettingRule :: Rule effs Splice (Doc a)
typeSettingRule = fromFunction "typesetting" go where
  go :: Splice -> Doc a
  go (Insert t)                 = pretty t
  go (Directive SoftWrap)       = softline
  go (Directive (HardWrap 0 _)) = line
  go (Directive (HardWrap i t)) = line <> stimes i (space t)
  go (Directive Don't)          = mempty
  space Space = " "
  space Tab   = "\t"
