{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Renderer.Test (test) where

import Data.Bifunctor.Join
import Data.Foldable
import Data.Functor.Foldable
import Data.Record
import Data.Text
import Prologue hiding (toList, intercalate)

import Category as C
import Diff
import Renderer
import Patch
import Info
import Syntax
import Term

test :: HasField fields Category => Renderer (Record fields)
test _ diff = TestOutput $ syntaxDiffToText diff

syntaxDiffToText :: HasField fields Category => Diff (Syntax Text) (Record fields) -> Text
syntaxDiffToText = cata algebra
  where
    algebra diff = case diff of
      (Pure patch) -> patchFields patch
      (Free (Join (_, annotation) :< syntax)) -> "(" <> categoryName annotation <> unwords (toList syntax) <> ")"

patchFields :: HasField fields Category => Patch (Term (Syntax Text) (Record fields)) -> Text
patchFields patch = case patch of
  Insert term -> fields "+" term
  Delete term -> fields "-" term
  Replace a b -> "(" <> termFields a <> "->" <> termFields b <> ")"
  where
    fields kind term = "(" <> kind <> termFields term <> ")"

termFields :: HasField fields Category => Term (Syntax Text) (Record fields) -> Text
termFields = cata algebra
  where
    algebra term = case term of
      (annotation :< Leaf _) -> categoryName annotation
      (annotation :< syntax) -> categoryName annotation <> "(" <> unwords (toList syntax) <> ")"

categoryName :: HasField fields Category => Record fields -> Text
categoryName = toLower . toS . category
