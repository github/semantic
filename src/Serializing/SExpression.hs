{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Serializing.SExpression
( serializeSExpression
, ToSExpression(..)
, Options(..)
) where

import Analysis.ConstructorName
import Data.ByteString.Builder
import Data.Functor.Foldable
import Data.Term

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
