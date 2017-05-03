module Data.Syntax.Expression where

import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callFunction :: a, callParams :: [a] }
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Call where liftEq = genericLiftEq
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec


-- | Binary arithmetic operators.
data Arithmetic a
  = Plus a a
  | Minus a a
  | Times a a
  | DividedBy a a
  | Power a a
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Arithmetic where liftEq = genericLiftEq
instance Show1 Arithmetic where liftShowsPrec = genericLiftShowsPrec

-- | Boolean operators.
data Boolean a
  = Or a a
  | And a a
  | Not a
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec
