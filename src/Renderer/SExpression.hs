{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverloadedStrings #-}
module Renderer.SExpression (sExpression, printTerm, printTerms, SExpressionFormat(..)) where

import Data.Bifunctor.Join
import Data.ByteString hiding (foldr, spanEnd)
import Data.Functor.Both
import Data.Record
import Prologue hiding (replicate, encodeUtf8)
import Category as C
import Diff
import Patch
import Info
import Source
import Syntax
import Term

data SExpressionFormat = TreeOnly | TreeAndRanges
  deriving (Show)

sExpression :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> Both SourceBlob -> Diff (Syntax Text) (Record fields) -> ByteString
sExpression format _ diff = printDiff diff 0 format

printDiff :: (HasField fields Category, HasField fields SourceSpan) => Diff (Syntax Text) (Record fields) -> Int -> SExpressionFormat -> ByteString
printDiff diff level format = case runFree diff of
  (Pure patch) -> case patch of
    Insert term -> pad (level - 1) <> "{+" <> printTerm term level format <> "+}"
    Delete term -> pad (level - 1) <> "{-" <> printTerm term level format <> "-}"
    Replace a b -> pad (level - 1) <> "{ " <> printTerm a level format <> pad (level - 1) <> "->" <> printTerm b level format <> " }"
  (Free (Join (_, annotation) :< syntax)) -> pad' level <> "(" <> showAnnotation annotation format <> foldr (\d acc -> printDiff d (level + 1) format <> acc) "" syntax <> ")"
  where
    pad' :: Int -> ByteString
    pad' n = if n < 1 then "" else pad n
    pad :: Int -> ByteString
    pad n | n < 0 = ""
          | n < 1 = "\n"
          | otherwise = "\n" <> replicate (2 * n) space

printTerms :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> [Term (Syntax t) (Record fields)] -> ByteString
printTerms format terms = foldr (\t acc -> printTerm t 0 format <> acc) "" terms

printTerm :: (HasField fields Category, HasField fields SourceSpan) => Term (Syntax t) (Record fields) -> Int -> SExpressionFormat -> ByteString
printTerm term level format = go term level 0
  where
    pad :: Int -> Int -> ByteString
    pad p n | n < 1 = ""
            | otherwise = "\n" <> replicate (2 * (p + n)) space
    go :: (HasField fields Category, HasField fields SourceSpan) => Term (Syntax t) (Record fields) -> Int -> Int -> ByteString
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

space :: Word8
space = fromIntegral $ ord ' '
