module Data.Coalescent where

-- | The class of types which can optionally be coalesced together.
class Coalescent a where
  coalesce :: a -> a -> Maybe a
