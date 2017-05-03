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


-- | Unary boolean negation, like '!x' in many languages.
data Not a = Not a
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Not where liftEq = genericLiftEq
instance Show1 Not where liftShowsPrec = genericLiftShowsPrec


-- | Binary addition.
data Plus a = Plus a a
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Plus where liftEq = genericLiftEq
instance Show1 Plus where liftShowsPrec = genericLiftShowsPrec

-- | Binary subtraction.
data Minus a = Minus a a
  deriving (Eq, Foldable, Functor, Generic1, Show, Traversable)

instance Eq1 Minus where liftEq = genericLiftEq
instance Show1 Minus where liftShowsPrec = genericLiftShowsPrec
