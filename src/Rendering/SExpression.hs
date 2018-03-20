{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Rendering.SExpression
( renderSExpressionDiff
, renderSExpressionTerm
) where

import Prologue
import Data.ByteString.Char8
import Data.Diff
import Data.Patch
import Data.Record
import Data.Term
import Prelude hiding (replicate)

-- | Returns a ByteString SExpression formatted diff.
renderSExpressionDiff :: (ConstrainAll Show fields, Foldable syntax, Functor syntax) => Diff syntax (Record fields) (Record fields) -> ByteString
renderSExpressionDiff diff = cata printDiffF diff 0 <> "\n"

-- | Returns a ByteString SExpression formatted term.
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable syntax, Functor syntax) => Term syntax (Record fields) -> ByteString
renderSExpressionTerm term = cata (\ term n -> nl n <> replicate (2 * n) ' ' <> printTermF term n) term 0 <> "\n"

printDiffF :: (ConstrainAll Show fields, Foldable syntax) => DiffF syntax (Record fields) (Record fields) (Int -> ByteString) -> Int -> ByteString
printDiffF diff n = case diff of
  Patch (Delete term) -> nl n <> pad (n - 1) <> "{-" <> printTermF term n <> "-}"
  Patch (Insert term) -> nl n <> pad (n - 1) <> "{+" <> printTermF term n <> "+}"
  Patch (Replace term1 term2) -> nl n       <> pad (n - 1) <> "{ " <> printTermF term1 n
                              <> nl (n + 1) <> pad (n - 1) <> "->" <> printTermF term2 n <> " }"
  Merge (In (_, ann) syntax) -> nl n <> pad n <> "(" <> showAnnotation ann <> foldMap (\ d -> d (n + 1)) syntax <> ")"

printTermF :: (ConstrainAll Show fields, Foldable syntax) => TermF syntax (Record fields) (Int -> ByteString) -> Int -> ByteString
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
