module Data.Functor.Classes.Ord.Generic where

-- | Generically-derivable lifting of the 'Ord' class to unary type constructors.
class GOrd1 f where
  -- | Lift a comparison function through the type constructor.
  --
  --   The function will usually be applied to a comparison function, but the more general type ensures that the implementation uses it to compare elements of the first container with elements of the second.
  gliftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering
