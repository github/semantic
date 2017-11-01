{-# LANGUAGE DeriveAnyClass #-}
module Language.Go.Syntax where

import Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import GHC.Generics

-- | Variadic arguments and parameters in Go (e.g. parameter: `param ...Type`, argument: `Type...`).
data Variadic a = Variadic { variadicContext :: [a], variadicIdentifier :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Variadic where liftEq = genericLiftEq
instance Ord1 Variadic where liftCompare = genericLiftCompare
instance Show1 Variadic where liftShowsPrec = genericLiftShowsPrec

-- | A default pattern in a Go select or switch statement (e.g. `switch { default: s() }`).
newtype DefaultPattern a = DefaultPattern { defaultPatternBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 DefaultPattern where liftEq = genericLiftEq
instance Ord1 DefaultPattern where liftCompare = genericLiftCompare
instance Show1 DefaultPattern where liftShowsPrec = genericLiftShowsPrec

-- | A rune literal in Go (e.g. `'⌘'`).
newtype RuneLiteral a = RuneLiteral { runeLiteralContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RuneLiteral where liftEq = genericLiftEq
instance Ord1 RuneLiteral where liftCompare = genericLiftCompare
instance Show1 RuneLiteral where liftShowsPrec = genericLiftShowsPrec

-- | A labeled statement in Go (e.g. `label:continue`).
data Label a = Label { labelName :: !a, labelStatement :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Label where liftEq = genericLiftEq
instance Ord1 Label where liftCompare = genericLiftCompare
instance Show1 Label where liftShowsPrec = genericLiftShowsPrec

-- | A send statement in Go (e.g. `channel <- value`).
data Send a = Send { sendReceiver :: !a, sendValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Send where liftEq = genericLiftEq
instance Ord1 Send where liftCompare = genericLiftCompare
instance Show1 Send where liftShowsPrec = genericLiftShowsPrec

-- | A defer statement in Go (e.g. `defer x()`).
newtype Defer a = Defer { deferBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Defer where liftEq = genericLiftEq
instance Ord1 Defer where liftCompare = genericLiftCompare
instance Show1 Defer where liftShowsPrec = genericLiftShowsPrec

-- | A go statement (i.e. go routine) in Go (e.g. `go x()`).
newtype Go a = Go { goBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Go where liftEq = genericLiftEq
instance Ord1 Go where liftCompare = genericLiftCompare
instance Show1 Go where liftShowsPrec = genericLiftShowsPrec

-- | A slice expression in Go (e.g. `a[1:4:3]` where a is a list, 1 is the low bound, 4 is the high bound, and 3 is the max capacity).
data Slice a = Slice { sliceName :: !a, sliceLow :: !a, sliceHigh :: !a, sliceCapacity :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Slice where liftEq = genericLiftEq
instance Ord1 Slice where liftCompare = genericLiftCompare
instance Show1 Slice where liftShowsPrec = genericLiftShowsPrec

-- | A select statement in Go (e.g. `select { case x := <-c: x() }` where each case is a send or receive operation on channels).
data Select a = Select { selectCases :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Select where liftEq = genericLiftEq
instance Ord1 Select where liftCompare = genericLiftCompare
instance Show1 Select where liftShowsPrec = genericLiftShowsPrec

-- | A communication clause in a Go select statement (e.g. `select { case x := <-c: x() }` where `case x:= <-c:` is the communication subject and `x()` is the communication body).
data Communication a = Communication { communicationSubject :: !a, communicationBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Communication where liftEq = genericLiftEq
instance Ord1 Communication where liftCompare = genericLiftCompare
instance Show1 Communication where liftShowsPrec = genericLiftShowsPrec

