{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Renderer.Test (test) where

import Data.Bifunctor.Join
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Both
import Data.Record
import Data.Text hiding (foldr)
import Prologue hiding (toList, intercalate)

import Category as C
import Diff
import Renderer
import Patch
import Info
import Syntax
import Term

test :: HasField fields Category => Renderer (Record fields)
test _ diff = TestOutput $ printDiff diff

printDiff :: HasField fields Category => Diff (Syntax Text) (Record fields) -> Text
printDiff diff = case runFree diff of
  (Pure patch) -> case patch of
    Insert term -> "(+" <> printTerm term <> ")"
    Delete term -> "(-" <> printTerm term <> ")"
    Replace a b -> "(" <> printTerm a <> "->" <> printTerm b <> ")"
  (Free (Join (_, annotation) :< syntax)) -> "(" <> categoryName annotation <> foldr (\a b -> printDiff a <> b) "" syntax <> ")"
  where
    printTerm term = case runCofree term of
      (annotation :< Leaf _) -> categoryName annotation
      (annotation :< syntax) -> categoryName annotation <> "(" <> foldr (\a b -> printTerm a <> b) "" syntax <> ")"




-- TODO: Move over to FDocs about how to understand structure of Diff as well as
-- the use of Free and Cofree on the different levels.
syntaxDiffToText :: HasField fields Category => Diff (Syntax Text) (Record fields) -> Text
syntaxDiffToText = cata algebra
  where
    algebra :: (HasField fields1 Category, HasField fields Category, Foldable t) => FreeF (TermF t (Both (Record fields1))) (Patch (Term (Syntax Text) (Record fields))) Text -> Text
    algebra diff = case diff of
      -- Pure nodes are patches (what's changed)
      (Pure patch) -> patchFields patch
      -- Free nodes are context
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
    algebra :: HasField fields Category => TermF (Syntax leaf) (Record fields) Text -> Text
    algebra term = case term of
      (annotation :< Leaf _) -> categoryName annotation
      (annotation :< syntax) -> categoryName annotation <> "(" <> unwords (toList syntax) <> ")"

categoryName :: HasField fields Category => Record fields -> Text
categoryName = toLower . toS . category
