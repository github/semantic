module Term where

import Control.Comonad.Cofree
import Data.Functor.Both
import Data.Maybe
import Data.OrderedMap hiding (size)
import Syntax

-- | An annotated node (Syntax) in an abstract syntax tree.
type Term a annotation = Cofree (Syntax a) annotation

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: Term a annotation -> Term a annotation -> Maybe (Term a (Both annotation))
zipTerms (annotation1 :< a) (annotation2 :< b) = annotate $ zipUnwrap a b
  where
    annotate = fmap (Both (annotation1, annotation2) :<)
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Fold a term into some other value, starting with the leaves.
cata :: (annotation -> Syntax a b -> b) -> Term a annotation -> b
cata f (annotation :< syntax) = f annotation $ cata f <$> syntax

-- | Return the number of leaves in the node.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size _ (Leaf _) = 1
  size _ (Indexed i) = sum i
  size _ (Fixed f) = sum f
  size _ (Keyed k) = sum k
