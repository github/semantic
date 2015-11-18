module Patch where

data Patch a =
  Replace a a
  | Insert a
  | Delete a
