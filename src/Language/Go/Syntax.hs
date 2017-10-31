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

-- | Variadic (`...Type` used in function declarations, and `Type...` used in function calls).
data Variadic a = Variadic { variadicContext :: [a], variadicIdentifier :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Variadic where liftEq = genericLiftEq
instance Ord1 Variadic where liftCompare = genericLiftCompare
instance Show1 Variadic where liftShowsPrec = genericLiftShowsPrec

-- | A pattern in a pattern-matching or computed jump control-flow statement, like 'case' in C or JavaScript, 'when' in Ruby, or the left-hand side of '->' in the body of Haskell 'case' expressions.
newtype DefaultPattern a = DefaultPattern { defaultPatternBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 DefaultPattern where liftEq = genericLiftEq
instance Ord1 DefaultPattern where liftCompare = genericLiftCompare
instance Show1 DefaultPattern where liftShowsPrec = genericLiftShowsPrec

-- | A rune literal in Go (e.g. '⌘')
newtype RuneLiteral a = RuneLiteral { runeLiteralContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RuneLiteral where liftEq = genericLiftEq
instance Ord1 RuneLiteral where liftCompare = genericLiftCompare
instance Show1 RuneLiteral where liftShowsPrec = genericLiftShowsPrec
