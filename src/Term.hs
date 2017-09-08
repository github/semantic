{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
module Term
( Term(..)
, TermF(..)
, SyntaxTerm
, SyntaxTermF
, zipTerms
, termSize
, alignTermWith
, cofree
, unTerm
, extract
, unwrap
, hoistCofree
) where

import Control.Comonad
import Control.Comonad.Cofree.Class
import Control.DeepSeq
import Control.Monad.Free
import Data.Align.Generic
import Data.Bifunctor
import Data.Functor.Both
import Data.Functor.Classes
import Data.Functor.Classes.Pretty.Generic as Pretty
import Data.Functor.Foldable
import Data.Functor.Listable
import Data.Maybe
import Data.Proxy
import Data.Record
import Data.These
import Data.Union
import Syntax

-- | A Term with an abstract syntax tree and an annotation.
infixr 5 :<
data Term f a = a :< f (Term f a)
infixr 5 :<<
data TermF f a b = (:<<) { headF :: a, tailF :: f b }
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | A Term with a Syntax leaf and a record of fields.
type SyntaxTerm fields = Term Syntax (Record fields)
type SyntaxTermF fields = TermF Syntax (Record fields)

instance (NFData (f (Term f a)), NFData a, Functor f) => NFData (Term f a) where
  rnf = rnf . unTerm

instance (NFData a, NFData (f b)) => NFData (TermF f a b) where
  rnf (a :<< s) = rnf a `seq` rnf s `seq` ()

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: (Traversable f, GAlign f) => Term f annotation -> Term f annotation -> Maybe (Term f (Both annotation))
zipTerms t1 t2 = iter go (alignTermWith galign (const Nothing) both (These t1 t2))
  where go (a :<< s) = (a :<) <$> sequenceA s

-- | Return the node count of a term.
termSize :: (Foldable f, Functor f) => Term f annotation -> Int
termSize = cata size where
  size (_ :<< syntax) = 1 + sum syntax

-- | Aligns (zips, retaining non-overlapping portions of the structure) a pair of terms.
alignTermWith :: Functor f
  => (forall a b. f a -> f b -> Maybe (f (These a b))) -- ^ A function comparing a pair of structures, returning `Just` the combined structure if they are comparable (e.g. if they have the same constructor), and `Nothing` otherwise. The 'Data.Align.Generic.galign' function is usually what you want here.
  -> (These (Term f a) (Term f b) -> contrasted) -- ^ A function mapping a 'These' of incomparable terms into 'Pure' values in the resulting tree.
  -> (a -> b -> combined) -- ^ A function mapping the input terms’ annotations into annotations in the 'Free' values in the resulting tree.
  -> These (Term f a) (Term f b) -- ^ The input terms.
  -> Free (TermF f combined) contrasted
alignTermWith compare contrast combine = go
  where go terms = fromMaybe (pure (contrast terms)) $ case terms of
          These (a1 :< f1) (a2 :< f2) -> wrap . (combine a1 a2 :<<) . fmap go <$> compare f1 f2
          _ -> Nothing


cofree :: TermF f a (Term f a) -> Term f a
cofree (a :<< f) = a :< f

unTerm :: Term f a -> TermF f a (Term f a)
unTerm (a :< f) = a :<< f

hoistCofree :: Functor f => (forall a. f a -> g a) -> Term f a -> Term g a
hoistCofree f = go where go (a :< r) = a :< f (fmap go r)

instance Pretty1 f => Pretty1 (Term f) where
  liftPretty p pl = go where go (a :< f) = p a <+> liftPretty go (Pretty.list . map (liftPretty p pl)) f

instance (Pretty1 f, Pretty a) => Pretty (Term f a) where
  pretty = liftPretty pretty prettyList

instance Apply1 Pretty1 fs => Pretty1 (Union fs) where
  liftPretty p pl = apply1 (Proxy :: Proxy Pretty1) (liftPretty p pl)

type instance Base (Term f a) = TermF f a

instance Functor f => Recursive (Term f a) where project = unTerm
instance Functor f => Corecursive (Term f a) where embed = cofree

instance Functor f => Comonad (Term f) where
  extract (a :< _) = a
  duplicate w = w :< fmap duplicate (unwrap w)
  extend f = go where go w = f w :< fmap go (unwrap w)

instance Functor f => Functor (Term f) where
  fmap f = go where go (a :< r) = f a :< fmap go r

instance Functor f => ComonadCofree f (Term f) where
  unwrap (_ :< as) = as
  {-# INLINE unwrap #-}

instance Eq1 f => Eq1 (Term f) where
  liftEq eqA = go where go (a1 :< f1) (a2 :< f2) = eqA a1 a2 && liftEq go f1 f2

instance (Eq1 f, Eq a) => Eq (Term f a) where
  (==) = eq1

instance Show1 f => Show1 (Term f) where
  liftShowsPrec spA slA = go where go d (a :< f) = showParen (d > 5) $ spA 6 a . showString " :< " . liftShowsPrec go (liftShowList spA slA) 5 f

instance (Show1 f, Show a) => Show (Term f a) where
  showsPrec = showsPrec1

instance Functor f => Bifunctor (TermF f) where
  bimap f g (a :<< r) = f a :<< fmap g r

instance Listable1 f => Listable2 (TermF f) where
  liftTiers2 annotationTiers recurTiers = liftCons2 annotationTiers (liftTiers recurTiers) (:<<)

instance (Listable1 f, Listable a) => Listable1 (TermF f a) where
  liftTiers = liftTiers2 tiers

instance (Functor f, Listable1 f) => Listable1 (Term f) where
  liftTiers annotationTiers = go
    where go = liftCons1 (liftTiers2 annotationTiers go) cofree

instance Eq1 f => Eq2 (TermF f) where
  liftEq2 eqA eqB (a1 :<< f1) (a2 :<< f2) = eqA a1 a2 && liftEq eqB f1 f2

instance (Eq1 f, Eq a) => Eq1 (TermF f a) where
  liftEq = liftEq2 (==)

instance Show1 f => Show2 (TermF f) where
  liftShowsPrec2 spA _ spB slB d (a :<< f) = showParen (d > 5) $ spA 6 a . showString " :<< " . liftShowsPrec spB slB 5 f
