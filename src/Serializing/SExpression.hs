{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Serializing.SExpression
( serializeSExpression
, ToSExpression(..)
) where

import Analysis.ConstructorName
import Data.ByteString.Builder
import Data.Diff
import Data.Patch
import Data.Term
import Prelude
import Prologue

serializeSExpression :: (Recursive t, ToSExpression (Base t)) => t -> Builder
serializeSExpression t = cata toSExpression t 0 <> "\n"

printTermF :: Foldable syntax => String -> syntax (Int -> Builder) -> Int -> Builder
printTermF name syntax n = "(" <> stringUtf8 name <> foldMap ($ (n + 1)) syntax <> ")"

nl :: Int -> Builder
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> Builder
pad n = stringUtf8 (replicate (2 * n) ' ')


class ToSExpression base where
  toSExpression :: base (Int -> Builder) -> (Int -> Builder)

instance (ConstructorName syntax, Foldable syntax) => ToSExpression (TermF syntax ann) where
  toSExpression (In _ syntax) n = nl n <> pad n <> printTermF (constructorName syntax) syntax n

instance (ConstructorName syntax, Foldable syntax) => ToSExpression (DiffF syntax ann1 ann2) where
  toSExpression diff n = case diff of
    Patch (Delete term) -> nl n <> pad (n - 1) <> "{-" <> printTermF (constructorName (termFOut term)) (termFOut term) n <> "-}"
    Patch (Insert term) -> nl n <> pad (n - 1) <> "{+" <> printTermF (constructorName (termFOut term)) (termFOut term) n <> "+}"
    Patch (Replace term1 term2) -> nl n       <> pad (n - 1) <> "{ " <> printTermF (constructorName (termFOut term1)) (termFOut term1) n
                                <> nl (n + 1) <> pad (n - 1) <> "->" <> printTermF (constructorName (termFOut term2)) (termFOut term2) n <> " }"
    Merge term -> nl n <> pad n <> "(" <> stringUtf8 (constructorName (termFOut term)) <> foldMap ($ (n + 1)) (termFOut term) <> ")"
