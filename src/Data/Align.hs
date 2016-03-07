module Data.Align where

class Functor f => Align f where
  nil :: f a
  align :: f a -> f b -> f (These a b)
  align a b = alignWith id a b
  alignWith :: (These a b -> c) -> f a -> f b -> f c
  alignWith f a b = f <$> align a b
