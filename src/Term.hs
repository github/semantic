module Term where

import OrderedMap hiding (size)
import Data.Maybe
import Control.Comonad.Cofree
import Syntax

type Term a annotation = Cofree (Syntax a) annotation

zipTerms :: Term a annotation -> Term a annotation -> Maybe (Term a (annotation, annotation))
zipTerms (annotation1 :< a) (annotation2 :< b) = annotate $ zipUnwrap a b
  where
    annotate = fmap ((annotation1, annotation2) :<)
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

cata :: (annotation -> Syntax a b -> b) -> Term a annotation -> b
cata f (annotation :< syntax) = f annotation $ cata f <$> syntax

termSize :: Term a annotation -> Integer
termSize term = cata size term where
  size _ (Leaf _) = 1
  size _ (Indexed i) = sum i
  size _ (Fixed f) = sum f
  size _ (Keyed k) = sum $ snd <$> toList k
