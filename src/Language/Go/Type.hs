{-# LANGUAGE DeriveAnyClass #-}
module Language.Go.Type where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import GHC.Generics

-- | A Bidirectional channel in Go (e.g. `chan`).
newtype BiDirectionalChannel a = BiDirectionalChannel { biDirectionalChannelElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 BiDirectionalChannel where liftEq = genericLiftEq
instance Ord1 BiDirectionalChannel where liftCompare = genericLiftCompare
instance Show1 BiDirectionalChannel where liftShowsPrec = genericLiftShowsPrec

-- | A Receive channel in Go (e.g. `<-chan`).
newtype ReceiveChannel a = ReceiveChannel { receiveChannelElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ReceiveChannel where liftEq = genericLiftEq
instance Ord1 ReceiveChannel where liftCompare = genericLiftCompare
instance Show1 ReceiveChannel where liftShowsPrec = genericLiftShowsPrec

-- | A Send channel in Go (e.g. `chan<-`).
newtype SendChannel a = SendChannel { sendChannelElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 SendChannel where liftEq = genericLiftEq
instance Ord1 SendChannel where liftCompare = genericLiftCompare
instance Show1 SendChannel where liftShowsPrec = genericLiftShowsPrec
