{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverloadedStrings #-}
module Renderer.SExpression
( sExpression
, sExpressionParseTree
, SExpressionFormat(..)
) where

import Data.Bifunctor.Join
import Data.ByteString hiding (foldr, spanEnd)
import Data.Record
import Prologue hiding (replicate, encodeUtf8)
import Category as C
import Diff
import Patch
import Info
import Term

data SExpressionFormat = TreeOnly | TreeAndRanges
  deriving (Show)

-- | Returns a ByteString SExpression formatted diff.
sExpression :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => SExpressionFormat -> Diff f (Record fields) -> ByteString
sExpression format diff = printDiff diff 0 format <> "\n"

-- | Returns a ByteString SExpression formatted term.
sExpressionParseTree :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => SExpressionFormat -> Term f (Record fields) -> ByteString
sExpressionParseTree format term = printTerm term 0 format <> "\n"

printDiff :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => Diff f (Record fields) -> Int -> SExpressionFormat -> ByteString
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

printTerm :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => Term f (Record fields) -> Int -> SExpressionFormat -> ByteString
printTerm term level format = go term level 0
  where
    pad :: Int -> Int -> ByteString
    pad p n | n < 1 = ""
            | otherwise = "\n" <> replicate (2 * (p + n)) space
    go :: (HasField fields Category, HasField fields SourceSpan, Foldable f) => Term f (Record fields) -> Int -> Int -> ByteString
    go term parentLevel level = case runCofree term of
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
