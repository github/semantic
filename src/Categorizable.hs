module Categorizable where

import Term
import Control.Comonad.Cofree
import Data.Set
import qualified Data.Text as T

class Categorizable a where
  categories :: a -> Set T.Text

instance Categorizable annotation => Categorizable (Term a annotation) where
  categories (annotation :< _) = categories annotation

comparable :: Categorizable a => a -> a -> Bool
comparable a b = catsA == catsB || (not . Data.Set.null $ intersection catsA catsB)
  where
    catsA = categories a
    catsB = categories b
