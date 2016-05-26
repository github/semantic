module Term where

import Prologue
import Control.Comonad.Cofree
import Data.Functor.Both
import Data.OrderedMap hiding (size)
import Syntax

-- | An annotated node (Syntax) in an abstract syntax tree.
type Term a annotation = Cofree (Syntax a) annotation

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: Term a annotation -> Term a annotation -> Maybe (Term a (Both annotation))
zipTerms (annotation1 :< a) (annotation2 :< b) = annotate $ zipUnwrap a b
  where
    annotate = fmap (both annotation1 annotation2 :<)
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Fold a term into some other value, starting with the leaves.
cata :: Functor f => (annotation -> f a -> a) -> Cofree f annotation -> a
cata f = uncurry f . second (fmap (cata f)) . unCofree

-- | Unfold a term and its annotation starting from a seed value.
ana :: Functor f => (a -> (annotation, f a)) -> a -> Cofree f annotation
ana f = uncurry (:<) . second (fmap (ana f)) . f

-- | A hylomorphism. Given an `a`, unfold and then refold into a `b`.
hylo :: Functor f => (annotation -> f b -> b) -> (a -> (annotation, f a)) -> a -> b
hylo phi psi = cata phi . ana psi

-- | Return the node count of a term.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size _ syntax = 1 + sum syntax
