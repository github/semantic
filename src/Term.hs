{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Term where

import Control.Comonad.Trans.Cofree
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.Maybe
import Data.OrderedMap hiding (size)
import Syntax

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

-- | An annotated node (Syntax) in an abstract syntax tree.
type TermF a annotation = CofreeF (Syntax a) annotation
type Term a annotation = Cofree (Syntax a) annotation

type instance Base (Cofree f a) = CofreeF f a
instance Functor f => Foldable.Foldable (Cofree f a) where project = runCofree

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: Term a annotation -> Term a annotation -> Maybe (Term a (Both annotation))
zipTerms t1 t2 = annotate (zipUnwrap a b)
  where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = fmap (cofree . (Both (annotation1, annotation2) :<))
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Fold a term into some other value, starting with the leaves.
-- cata :: (annotation -> Syntax a b -> b) -> Term a annotation -> b
-- cata f (annotation :< syntax) = f annotation $ cata f <$> syntax

-- | Return the node count of a term.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size (_ :< syntax) = 1 + sum syntax
