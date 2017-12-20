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

newtype Include a = Include a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Include where liftEq          = genericLiftEq
instance Ord1 Include where liftCompare    = genericLiftCompare
instance Show1 Include where liftShowsPrec = genericLiftShowsPrec

newtype IncludeOnce a = IncludeOnce a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 IncludeOnce where liftEq          = genericLiftEq
instance Ord1 IncludeOnce where liftCompare    = genericLiftCompare
instance Show1 IncludeOnce where liftShowsPrec = genericLiftShowsPrec

newtype ArrayElement a = ArrayElement a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ArrayElement where liftEq          = genericLiftEq
instance Ord1 ArrayElement where liftCompare    = genericLiftCompare
instance Show1 ArrayElement where liftShowsPrec = genericLiftShowsPrec

newtype GlobalDeclaration a = GlobalDeclaration [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 GlobalDeclaration where liftEq          = genericLiftEq
instance Ord1 GlobalDeclaration where liftCompare    = genericLiftCompare
instance Show1 GlobalDeclaration where liftShowsPrec = genericLiftShowsPrec

newtype SimpleVariable a = SimpleVariable a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 SimpleVariable where liftEq          = genericLiftEq
instance Ord1 SimpleVariable where liftCompare    = genericLiftCompare
instance Show1 SimpleVariable where liftShowsPrec = genericLiftShowsPrec


-- | TODO: Unify with TypeScript's PredefinedType
newtype CastType a = CastType { _castType :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 CastType where liftEq = genericLiftEq
instance Ord1 CastType where liftCompare = genericLiftCompare
instance Show1 CastType where liftShowsPrec = genericLiftShowsPrec

newtype ErrorControl a = ErrorControl a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ErrorControl where liftEq = genericLiftEq
instance Ord1 ErrorControl where liftCompare = genericLiftCompare
instance Show1 ErrorControl where liftShowsPrec = genericLiftShowsPrec

newtype Clone a = Clone a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Clone where liftEq = genericLiftEq
instance Ord1 Clone where liftCompare = genericLiftCompare
instance Show1 Clone where liftShowsPrec = genericLiftShowsPrec

newtype ShellCommand a = ShellCommand ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ShellCommand where liftEq = genericLiftEq
instance Ord1 ShellCommand where liftCompare = genericLiftCompare
instance Show1 ShellCommand where liftShowsPrec = genericLiftShowsPrec

-- | TODO: Combine with TypeScript update expression.
newtype Update a = Update { _updateSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec
