{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Renderer.SExpression
( renderSExpressionDiff
, renderSExpressionTerm
) where

import Data.Bifunctor.Join
import Data.ByteString.Char8 hiding (intersperse, foldr, spanEnd)
import Data.Foldable (fold)
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
renderSExpressionDiff diff = cata printDiffF diff 0 <> "\n"

-- | Returns a ByteString SExpression formatted term.
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable f, Functor f) => Term f (Record fields) -> ByteString
renderSExpressionTerm term = cata (\ term n -> nl n <> replicate (2 * n) ' ' <> printTermF term n) term 0 <> "\n"

printDiffF :: (ConstrainAll Show fields, Foldable f, Functor f) => DiffF f (Record fields) (Int -> ByteString) -> Int -> ByteString
printDiffF diff n = case diff of
  Patch patch -> case patch of
    Insert term -> nl n <> pad (n - 1) <> "{+" <> printTermF term n <> "+}"
    Delete term -> nl n <> pad (n - 1) <> "{-" <> printTermF term n <> "-}"
    Replace a b -> nl n <> pad (n - 1) <> "{ " <> printTermF a n
                <> nl n <> pad (n - 1) <> "->" <> printTermF b n <> " }"
  Copy vs (Join (_, annotation)) syntax -> nl n <> pad n <> "(" <> showBindings (fmap (\ b -> b n) <$> vs) <> showAnnotation annotation <> foldMap (\ d -> d (n + 1)) syntax <> ")"
  Var v -> nl n <> pad n <> showMetaVar v

printTermF :: (ConstrainAll Show fields, Foldable f, Functor f) => TermF f (Record fields) (Int -> ByteString) -> Int -> ByteString
printTermF (annotation :< syntax) n = "(" <> showAnnotation annotation <> foldMap (\t -> t (n + 1)) syntax <> ")"

nl :: Int -> ByteString
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> ByteString
pad n = replicate (2 * n) ' '


showAnnotation :: ConstrainAll Show fields => Record fields -> ByteString
showAnnotation Nil = ""
showAnnotation (only :. Nil) = pack (show only)
showAnnotation (first :. rest) = pack (show first) <> " " <> showAnnotation rest

showBindings :: [(MetaVar, ByteString)] -> ByteString
showBindings [] = ""
showBindings bindings = "[ " <> fold (intersperse "\n, " (showBinding <$> bindings)) <> " ]"
  where showBinding (var, val) = showMetaVar var <> "/" <> val

showMetaVar :: MetaVar -> ByteString
showMetaVar (MetaVar s) = pack ('$' : s)
