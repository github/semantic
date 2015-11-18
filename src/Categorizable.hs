module Categorizable where

import Data.Set

class Categorizable a where
  categories :: a -> Set String
