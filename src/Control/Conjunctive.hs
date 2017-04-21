module Control.Conjunctive where

import Prologue

infixl 4 <&>

class Alternative f => Conjunctive f where
  (<&>) :: f (a -> b) -> f a -> f b

  (<&) :: f a -> f b -> f a

  (&>) :: f a -> f b -> f b
