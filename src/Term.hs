module Term where

import Control.Comonad.Cofree
import Syntax
import Categorizable

type Term a annotation = Cofree (Syntax a) annotation

instance Categorizable annotation => Categorizable (Term a annotation) where
  categories (annotation :< _) = categories annotation
