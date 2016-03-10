module Data.Coalescent where

class Coalescent a where
  coalesce :: a -> a -> Maybe a
