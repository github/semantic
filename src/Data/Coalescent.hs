module Data.Coalescent where

-- | The class of types which can optionally be coalesced together.
class Coalescent a where
  -- | Returns either Just the combined value of its inputs, or Nothing if they cannot be combined.
  coalesce :: a -> a -> Maybe a
