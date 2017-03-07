{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Renderer.SExpression (sExpression, printTerm, SExpressionFormat(..)) where

import Data.Bifunctor.Join
import Data.Foldable
import Data.Record
import Data.ByteString hiding (foldr, replicate, spanEnd)
import Prologue hiding (toList, intercalate)

import Category as C
import Diff
import Renderer
import Patch
import Info
import Syntax
import Term

data SExpressionFormat = TreeOnly | TreeAndRanges

sExpression :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Renderer (Record fields)
sExpression format _ diff = SExpressionOutput $ printDiff diff 0 format <> "\n"

printDiff :: (HasField fields Category, HasField fields SourceSpan) => Diff (Syntax Text) (Record fields) -> Int -> SExpressionFormat -> ByteString
printDiff diff level format = case runFree diff of
  (Pure patch) -> case patch of
    Insert term -> pad (level - 1) <> "{+" <> printTerm term level format <> "+}"
    Delete term -> pad (level - 1) <> "{-" <> printTerm term level format <> "-}"
    Replace a b -> pad (level - 1) <> "{ " <> printTerm a level format <> pad (level - 1) <> "->" <> printTerm b level format <> " }"
  (Free (Join (_, annotation) :< syntax)) -> pad' level <> "(" <> showAnnotation annotation format <> foldr (\d acc -> printDiff d (level + 1) format <> acc) "" syntax <> ")"
  where
    pad' n = if n < 1 then "" else pad n
    pad n | n < 0 = ""
          | n < 1 = "\n"
          | otherwise = "\n" <> mconcat (replicate n "  ")

printTerm :: (HasField fields Category, HasField fields SourceSpan) => Term (Syntax t) (Record fields) -> Int -> SExpressionFormat -> ByteString
printTerm term level format = go term level 0
  where
    pad p n | n < 1 = ""
            | otherwise = "\n" <> mconcat (replicate (p + n) "  ")
    go term parentLevel level = case runCofree term of
      (annotation :< Leaf _) -> pad parentLevel level <> "(" <> showAnnotation annotation format <> ")"
      (annotation :< syntax) -> pad parentLevel level <> "(" <> showAnnotation annotation format <> foldr (\t acc -> go t parentLevel (level + 1) <> acc) "" syntax <> ")"

showAnnotation :: (HasField fields Category, HasField fields SourceSpan) => Record fields -> SExpressionFormat -> ByteString
showAnnotation annotation TreeOnly = categoryName annotation
showAnnotation annotation TreeAndRanges = categoryName annotation <> " " <> showSourceSpan annotation
  where
    showSourceSpan a = start a <> " - " <> end a
    start = showPoint . spanStart . getField
    end = showPoint . spanEnd . getField
    showPoint SourcePos{..} = "[" <> show line <> ", " <> show column <> "]"

categoryName :: HasField fields Category => Record fields -> ByteString
categoryName = toS . category
