{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Expression where

import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callFunction :: !a, callParams :: ![a] }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Call where liftEq = genericLiftEq
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec


data Comparison a
  = LessThan !a !a
  | LessThanEqual !a !a
  | GreaterThan !a !a
  | GreaterThanEqual !a !a
  | Equal !a !a
  | Comparison !a !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Comparison where liftEq = genericLiftEq
instance Show1 Comparison where liftShowsPrec = genericLiftShowsPrec


-- | Binary arithmetic operators.
data Arithmetic a
  = Plus !a !a
  | Minus !a !a
  | Times !a !a
  | DividedBy !a !a
  | Modulo !a !a
  | Power !a !a
  | Negate !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Arithmetic where liftEq = genericLiftEq
instance Show1 Arithmetic where liftShowsPrec = genericLiftShowsPrec

-- | Boolean operators.
data Boolean a
  = Or !a !a
  | And !a !a
  | Not !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

-- | Bitwise operators.
data Bitwise a
  = BOr !a !a
  | BAnd !a !a
  | BXOr !a !a
  | LShift !a !a
  | RShift !a !a
  | Complement a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Bitwise where liftEq = genericLiftEq
instance Show1 Bitwise where liftShowsPrec = genericLiftShowsPrec

-- | Member Access (e.g. a.b)
data MemberAccess a
  = MemberAccess !a !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 MemberAccess where liftEq = genericLiftEq
instance Show1 MemberAccess where liftShowsPrec = genericLiftShowsPrec

-- | Subscript (e.g a[1])
data Subscript a
  = Subscript !a ![a]
  | Member !a !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Subscript where liftEq = genericLiftEq
instance Show1 Subscript where liftShowsPrec = genericLiftShowsPrec

-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
data ScopeResolution a
  = ScopeResolution ![a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ScopeResolution where liftEq = genericLiftEq
instance Show1 ScopeResolution where liftShowsPrec = genericLiftShowsPrec
