module Term where

import Control.Comonad.Cofree
import Syntax
import Categorizable

type Term a annotation = Cofree (Syntax a) annotation

instance Categorizable annotation => Categorizable (Term a annotation) where
  categories (annotation :< _) = categories annotation

zipTerms :: Term a annotation -> Term a annotation -> Maybe (Cofree (Syntax a) (annotation, annotation))
zipTerms (annotation1 :< a) (annotation2 :< b) = zipUnwrap a b
  where
    annotations = (annotation1, annotation2)
    zipUnwrap a b = Nothing
