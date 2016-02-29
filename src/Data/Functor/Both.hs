module Data.Functor.Both where

newtype Both a = Both { runBoth :: (a, a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Both where
  pure a = Both (a, a)
  Both (f, g) <*> Both (a, b) = Both (f a, g b)

instance Monoid a => Monoid (Both a) where
  mempty = pure mempty
  mappend a b = pure mappend <*> a <*> b
