module Term where

import Data.Map
import Data.Maybe
import Control.Comonad.Cofree
import Syntax
import Categorizable

type Term a annotation = Cofree (Syntax a) annotation

instance Categorizable annotation => Categorizable (Term a annotation) where
  categories (annotation :< _) = categories annotation

zipTerms :: Term a annotation -> Term a annotation -> Maybe (Cofree (Syntax a) (annotation, annotation))
zipTerms (annotation1 :< a) (annotation2 :< b) = annotate $ zipUnwrap a b
  where
    annotate = fmap ((annotation1, annotation2) :<)
    zipUnwrap (Leaf a) (Leaf b) = Just $ Leaf b
    zipUnwrap (Indexed a) (Indexed b) = Just . Indexed . catMaybes $ zipWith zipTerms a b
    zipUnwrap (Fixed a) (Fixed b) = Just . Fixed . catMaybes $ zipWith zipTerms a b
    zipUnwrap (Keyed a) (Keyed b) | keys a == keys b = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a b <$> keys a
    zipUnwrap a b = Nothing
    zipUnwrapMaps a b key = (,) key <$> zipTerms (a ! key) (b ! key)

cata :: (annotation -> Syntax a b -> b) -> Term a annotation -> b
cata f (annotation :< syntax) = f annotation $ cata f <$> syntax
