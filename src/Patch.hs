module Patch where

data Patch a =
  Replace a a
  | Insert a
  | Delete a
  deriving (Functor, Show, Eq)
