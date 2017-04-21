module Control.Conjunctive where

import Prologue

infixl 4 <&>

class Alternative f => Conjunctive f where
  (<&>) :: f (a -> b) -> f a -> f b
  f <&> a = liftC2 ($) f a

  liftC2 :: (a -> b -> c) -> f a -> f b -> f c
  liftC2 f a b = f <$> a <*> b

  (<&) :: f a -> f b -> f a
  a <& b = const <$> a <&> b

  (&>) :: f a -> f b -> f b
  a &> b = identity <$ a <&> b

  {-# MINIMAL (<&>) | liftC2 #-}
