module Categorizable where

import Term
import Control.Comonad.Cofree
import Data.Set

class Categorizable a where
  categories :: a -> Set String

instance Categorizable annotation => Categorizable (Term a annotation) where
  categories (annotation :< _) = categories annotation

comparable :: Categorizable a => a -> a -> Bool
comparable a b = Data.Set.null $ intersection (categories a) (categories b)
