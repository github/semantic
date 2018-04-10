{-# LANGUAGE DeriveAnyClass #-}
module Language.Java.Syntax where

import Data.Abstract.Evaluatable hiding (Label)
import Diffing.Algorithm
import Prologue

newtype ArrayType a = ArrayType ByteString
  deriving (Diffable, Eq, FreeVariables1, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable ArrayType
