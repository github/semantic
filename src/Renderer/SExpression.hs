{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Renderer.SExpression
( renderSExpressionDiff
, renderSExpressionTerm
) where

import Data.Bifunctor.Join
import Data.ByteString.Char8 hiding (intersperse, foldr, spanEnd)
import Data.Functor.Foldable (cata)
import Data.List (intersperse)
import Data.Record
import Data.Semigroup
import Diff
import Patch
import Prelude hiding (replicate)
import Term

-- | Returns a ByteString SExpression formatted diff.
renderSExpressionDiff :: (ConstrainAll Show fields, Foldable f, Functor f) => Diff f (Record fields) -> ByteString
renderSExpressionDiff diff = printDiff diff 0 <> "\n"

-- | Returns a ByteString SExpression formatted term.
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable f, Functor f) => Term f (Record fields) -> ByteString
renderSExpressionTerm term = printTerm term 0 <> "\n"

printDiff :: (ConstrainAll Show fields, Foldable f, Functor f) => Diff f (Record fields) -> Int -> ByteString
printDiff = cata $ \ diff level -> case diff of
  Patch patch -> case patch of
    Insert term -> pad (level - 1) <> "{+" <> printTermF term level <> "+}"
    Delete term -> pad (level - 1) <> "{-" <> printTermF term level <> "-}"
    Replace a b -> pad (level - 1) <> "{ " <> printTermF a level <> pad (level - 1) <> "->" <> printTermF b level <> " }"
  Copy vs (Join (_, annotation)) syntax -> pad' level <> "(" <> showBindings (fmap ($ 0) <$> vs) <> showAnnotation annotation <> foldr (\d acc -> d (level + 1) <> acc) "" syntax <> ")"
  Var v -> pad' level <> showMetaVar v
  where
    pad' :: Int -> ByteString
    pad' n = if n < 1 then "" else pad n

printTerm :: (ConstrainAll Show fields, Foldable f, Functor f) => Term f (Record fields) -> Int -> ByteString
printTerm term level = cata printTermF term level

printTermF :: (ConstrainAll Show fields, Foldable f, Functor f) => TermF f (Record fields) (Int -> ByteString) -> Int -> ByteString
printTermF (annotation :< syntax) level =
  pad level <> "(" <> showAnnotation annotation <> foldr (\t -> (t (level + 1) <>)) "" syntax <> ")"

pad :: Int -> ByteString
pad n | n <= 0 = ""
      | otherwise = "\n" <> replicate (2 * n) ' '


showAnnotation :: ConstrainAll Show fields => Record fields -> ByteString
showAnnotation Nil = ""
showAnnotation (only :. Nil) = pack (show only)
showAnnotation (first :. rest) = pack (show first) <> " " <> showAnnotation rest

showBindings :: [(MetaVar, ByteString)] -> ByteString
showBindings [] = ""
showBindings bindings = "[ " <> foldr (<>) "" (intersperse "\n, " (showBinding <$> bindings)) <> " ]"
  where showBinding (var, val) = showMetaVar var <> "/" <> val

showMetaVar :: MetaVar -> ByteString
showMetaVar (MetaVar s) = pack ('$' : s)
