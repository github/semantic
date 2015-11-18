module Categorizable where

import Data.Set

class Categorizable a where
  categories :: a -> Set String

comparable :: Categorizable a => a -> a -> Bool
comparable a b = Data.Set.null $ intersection (categories a) (categories b)
