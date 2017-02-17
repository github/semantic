{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Renderer.SExpression (sExpression, printTerm) where

import Data.Bifunctor.Join
import Data.Foldable
import Data.Record
import Data.Text hiding (foldr, replicate)
import Prologue hiding (toList, intercalate)

import Category as C
import Diff
import Renderer
import Patch
import Info
import Syntax
import Term

sExpression :: (HasField fields Category, HasField fields SourceSpan) => Renderer (Record fields)
sExpression _ diff = SExpressionOutput $ printDiff diff 0

printDiff :: (HasField fields Category, HasField fields SourceSpan) => Diff (Syntax Text) (Record fields) -> Int -> Text
printDiff diff level = case runFree diff of
  (Pure patch) -> case patch of
    Insert term -> pad (level - 1) <> "{+" <> printTerm term level <> "+}"
    Delete term -> pad (level - 1) <> "{-" <> printTerm term level <> "-}"
    Replace a b -> pad (level - 1) <> "{" <> printTerm a level <> "->" <> printTerm b level <> "}"
  (Free (Join (_, annotation) :< syntax)) -> pad' level <> "(" <> showAnnotation annotation <> foldr (\d acc -> printDiff d (level + 1) <> acc) "" syntax <> ")"
  where
    pad' n = if n < 1 then "" else pad n
    pad n | n < 1 = "\n"
          | otherwise = "\n" <> mconcat (replicate n "  ")

printTerm :: (HasField fields Category, HasField fields SourceSpan) => Term (Syntax t) (Record fields) -> Int -> Text
printTerm term level = go term level 0
  where
    pad p n | n < 1 = ""
            | otherwise = "\n" <> mconcat (replicate (p + n) "  ")
    go term parentLevel level = case runCofree term of
      (annotation :< Leaf _) -> pad parentLevel level <> "(" <> showAnnotation annotation <> ")"
      (annotation :< syntax) -> pad parentLevel level <> "(" <> showAnnotation annotation <> foldr (\t acc -> go t parentLevel (level + 1) <> acc) "" syntax <> ")"

showAnnotation :: (HasField fields Category, HasField fields SourceSpan) => Record fields -> Text
showAnnotation annotation = categoryName annotation <> " " <> showSourceSpan annotation
  where
    showSourceSpan a = start a <> " - " <> end a
    start = showPoint . spanStart . getField
    end = showPoint . spanEnd . getField
    showPoint SourcePos{..} = "[" <> show line <> ", " <> show column <> "]"

categoryName :: HasField fields Category => Record fields -> Text
categoryName = toS . category
