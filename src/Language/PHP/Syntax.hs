{-# LANGUAGE DeriveAnyClass #-}
module Language.PHP.Syntax where

import Diffing.Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Generic
import Data.Mergeable
import GHC.Generics

newtype Text a = Text ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Ord1 Text where liftCompare = genericLiftCompare
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

newtype VariableName a = VariableName a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 VariableName where liftEq = genericLiftEq
instance Ord1 VariableName where liftCompare = genericLiftCompare
instance Show1 VariableName where liftShowsPrec = genericLiftShowsPrec

newtype RequireOnce a = RequireOnce a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RequireOnce where liftEq = genericLiftEq
instance Ord1 RequireOnce where liftCompare = genericLiftCompare
instance Show1 RequireOnce where liftShowsPrec = genericLiftShowsPrec

newtype Require a = Require a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Require where liftEq          = genericLiftEq
instance Ord1 Require where liftCompare    = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec
