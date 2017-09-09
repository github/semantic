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
renderSExpressionDiff diff = cata printDiffF diff 0 0 <> "\n"

-- | Returns a ByteString SExpression formatted term.
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable f, Functor f) => Term f (Record fields) -> ByteString
renderSExpressionTerm term = cata printTermF term 0 0 <> "\n"

printDiffF :: (ConstrainAll Show fields, Foldable f, Functor f) => DiffF f (Record fields) (Int -> Int -> ByteString) -> Int -> Int -> ByteString
printDiffF diff parentLevel level = case diff of
  Patch patch -> case patch of
    Insert term -> pad parentLevel (level - 1) <> "{+" <> printTermF term (parentLevel + level) 0 <> "+}"
    Delete term -> pad parentLevel (level - 1) <> "{-" <> printTermF term (parentLevel + level) 0 <> "-}"
    Replace a b -> pad parentLevel (level - 1) <> "{ " <> printTermF a (parentLevel + level) 0 <> pad parentLevel (level - 1) <> "->" <> printTermF b (parentLevel + level) 0 <> " }"
  Copy vs (Join (_, annotation)) syntax -> pad parentLevel level <> "(" <> showBindings (fmap (($ level) . ($ parentLevel)) <$> vs) <> showAnnotation annotation <> foldr (\d -> (d parentLevel (level + 1) <>)) "" syntax <> ")"
  Var v -> pad parentLevel level <> showMetaVar v

printTermF :: (ConstrainAll Show fields, Foldable f, Functor f) => TermF f (Record fields) (Int -> Int -> ByteString) -> Int -> Int -> ByteString
printTermF (annotation :< syntax) parentLevel level =
  pad parentLevel level <> "(" <> showAnnotation annotation <> foldr (\t -> (t parentLevel (level + 1) <>)) "" syntax <> ")"

pad :: Int -> Int -> ByteString
pad p n | n <= 0    = ""
        | otherwise = "\n" <> replicate (2 * (p + n)) ' '


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
