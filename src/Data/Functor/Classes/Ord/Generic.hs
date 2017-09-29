module Data.Functor.Classes.Ord.Generic where
import GHC.Generics

-- | Generically-derivable lifting of the 'Ord' class to unary type constructors.
class GOrd1 f where
  -- | Lift a comparison function through the type constructor.
  --
  --   The function will usually be applied to a comparison function, but the more general type ensures that the implementation uses it to compare elements of the first container with elements of the second.
  gliftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | A suitable implementation of Ord1’s liftCompare for Generic1 types.
genericLiftCompare :: (Generic1 f, GOrd1 (Rep1 f)) => (a -> b -> Ordering) -> f a -> f b -> Ordering
genericLiftCompare f a b = gliftCompare f (from1 a) (from1 b)
