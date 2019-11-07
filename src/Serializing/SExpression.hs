{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Serializing.SExpression
( serializeSExpression
, ToSExpression(..)
, Options(..)
) where

import Analysis.ConstructorName
import Data.ByteString.Builder
import Data.Diff
import Data.Edit
import Data.Term
import Prelude
import Prologue

data Options = ByShow | ByConstructorName

serializeSExpression :: (Recursive t, ToSExpression (Base t)) => Options -> t -> Builder
serializeSExpression options t = cata (toSExpression options) t 0 <> "\n"

branch :: Foldable syntax => String -> syntax (Int -> Builder) -> Int -> Builder
branch name syntax n = "(" <> stringUtf8 name <> foldMap ($ (n + 1)) syntax <> ")"

namedBranch :: (ConstructorName syntax, Foldable syntax, Show ann) => Options -> TermF syntax ann (Int -> Builder) -> Int -> Builder
namedBranch ByShow            (In ann syntax) = branch (show ann) syntax
namedBranch ByConstructorName (In _   syntax) = branch (constructorName syntax) syntax

nl :: Int -> Builder
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> Builder
pad n = stringUtf8 (replicate (2 * n) ' ')


class ToSExpression base where
  toSExpression :: Options -> base (Int -> Builder) -> (Int -> Builder)

instance (ConstructorName syntax, Foldable syntax, Show ann) => ToSExpression (TermF syntax ann) where
  toSExpression options term n = nl n <> pad n <> namedBranch options term n

instance (ConstructorName syntax, Foldable syntax, Show ann1, Show ann2) => ToSExpression (DiffF syntax ann1 ann2) where
  toSExpression options diff n = case diff of
    Patch (Delete term) -> nl n <> pad (n - 1) <> "{-" <> namedBranch options term n <> "-}"
    Patch (Insert term) -> nl n <> pad (n - 1) <> "{+" <> namedBranch options term n <> "+}"
    Patch (Compare term1 term2) -> nl n       <> pad (n - 1) <> "{ " <> namedBranch options term1 n
                                <> nl (n + 1) <> pad (n - 1) <> "->" <> namedBranch options term2 n <> " }"
    Merge term -> nl n <> pad n <> namedBranch options term n
