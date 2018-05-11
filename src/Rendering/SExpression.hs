{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Rendering.SExpression
( renderSExpressionDiff
, renderSExpressionTerm
, renderSExpressionAST
) where

import Prologue
import Data.ByteString.Char8
import Data.Diff
import Data.Patch
import Data.Record
import Data.AST
import Data.Term
import Prelude hiding (replicate)

-- | Returns a ByteString SExpression formatted diff.
renderSExpressionDiff :: (ConstrainAll Show fields, Foldable syntax, Functor syntax) => Diff syntax (Record fields) (Record fields) -> ByteString
renderSExpressionDiff diff = cata printDiffF diff 0 <> "\n"

-- | Returns a ByteString SExpression formatted term (generalized).
renderSExpressionTerm :: (ConstrainAll Show fields, Foldable syntax, Functor syntax) => Term syntax (Record fields) -> ByteString
renderSExpressionTerm = toSExpression showRecord

-- | Returns a ByteString SExpression formatted term (specialized)
renderSExpressionAST :: Show grammar => Term [] (Node grammar) -> ByteString
renderSExpressionAST = toSExpression (pack . show . nodeSymbol)


toSExpression :: (Base t ~ TermF syntax ann, Foldable syntax, Recursive t) => (ann -> ByteString) -> t -> ByteString
toSExpression showAnn term = cata (\ term n -> nl n <> replicate (2 * n) ' ' <> printTermF showAnn term n) term 0 <> "\n"

printDiffF :: (ConstrainAll Show fields, Foldable syntax) => DiffF syntax (Record fields) (Record fields) (Int -> ByteString) -> Int -> ByteString
printDiffF diff n = case diff of
  Patch (Delete term) -> nl n <> pad (n - 1) <> "{-" <> printTermF showRecord term n <> "-}"
  Patch (Insert term) -> nl n <> pad (n - 1) <> "{+" <> printTermF showRecord term n <> "+}"
  Patch (Replace term1 term2) -> nl n       <> pad (n - 1) <> "{ " <> printTermF showRecord term1 n
                              <> nl (n + 1) <> pad (n - 1) <> "->" <> printTermF showRecord term2 n <> " }"
  Merge (In (_, ann) syntax) -> nl n <> pad n <> "(" <> showRecord ann <> foldMap (\ d -> d (n + 1)) syntax <> ")"

printTermF :: Foldable syntax => (ann -> ByteString) -> TermF syntax ann (Int -> ByteString) -> Int -> ByteString
printTermF f (In ann syntax) n = "(" <> f ann <> foldMap (\t -> t (succ n)) syntax <> ")"

nl :: Int -> ByteString
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> ByteString
pad n = replicate (2 * n) ' '

showRecord :: ConstrainAll Show fields => Record fields -> ByteString
showRecord Nil = ""
showRecord (only :. Nil) = pack (show only)
showRecord (first :. rest) = pack (show first) <> " " <> showRecord rest
