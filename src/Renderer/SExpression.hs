{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Renderer.SExpression
( renderSExpressionDiff
, renderSExpressionTerm
) where

import Data.Bifunctor (bimap)
import Data.ByteString.Char8 hiding (intersperse, foldr, spanEnd, length)
import Data.Foldable (fold)
import Data.Functor.Binding (BindingF(..), Env(..), Metavar(..))
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
renderSExpressionDiff diff = cata printBindingF diff 0 <> "\n"

-- | Returns a ByteString SExpression formatted term.
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable f, Functor f) => Term f (Record fields) -> ByteString
renderSExpressionTerm term = cata (\ term n -> nl n <> replicate (2 * n) ' ' <> printTermF term n) term 0 <> "\n"

printBindingF :: (ConstrainAll Show fields, Foldable f, Functor f) => BindingF (DiffF f (Record fields)) (Int -> ByteString) -> Int -> ByteString
printBindingF bind n = case bind of
  Let vars body -> nl n <> pad n <> "(" <> showBindings (($ n) <$> vars) <> printDiffF body n <> ")"
  Var v -> nl n <> pad n <> showMetavar v

printDiffF :: (ConstrainAll Show fields, Foldable f, Functor f) => DiffF f (Record fields) (Int -> ByteString) -> Int -> ByteString
printDiffF diff n = case diffF diff of
  Left (Delete term) -> nl n <> pad (n - 1) <> "{-" <> printTermF term n <> "-}"
  Left (Insert term) -> nl n <> pad (n - 1) <> "{+" <> printTermF term n <> "+}"
  Left (Replace term1 term2) -> nl n       <> pad (n - 1) <> "{ " <> printTermF term1 n
                             <> nl (n + 1) <> pad (n - 1) <> "->" <> printTermF term2 n <> " }"
  Right (In (_, ann) syntax) -> nl n <> pad n <> "(" <> showAnnotation ann <> foldMap (\ d -> d (n + 1)) syntax <> ")"

printTermF :: (ConstrainAll Show fields, Foldable f, Functor f) => TermF f (Record fields) (Int -> ByteString) -> Int -> ByteString
printTermF (In annotation syntax) n = "(" <> showAnnotation annotation <> foldMap (\t -> t (n + 1)) syntax <> ")"

nl :: Int -> ByteString
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> ByteString
pad n = replicate (2 * n) ' '


showAnnotation :: ConstrainAll Show fields => Record fields -> ByteString
showAnnotation Nil = ""
showAnnotation (only :. Nil) = pack (show only)
showAnnotation (first :. rest) = pack (show first) <> " " <> showAnnotation rest

showBindings :: Env ByteString -> ByteString
showBindings (Env []) = ""
showBindings (Env bindings) = "[ " <> fold (intersperse "\n, " (showBinding <$> bindings)) <> " ]"
  where showBinding (var, val) = showMetavar var <> "/" <> val

showMetavar :: Metavar -> ByteString
showMetavar (Metavar i) = pack (toName i)
  where toName i | i < 0 = ""
                 | otherwise = uncurry (++) (bimap (toName . pred) (pure . (alphabet !!)) (i `divMod` la))
        alphabet = ['a'..'z']
        la = length alphabet
