module Data.Copointed where

class Copointed c where
  copoint :: c a -> a

instance Copointed ((,) a) where
  copoint = snd
