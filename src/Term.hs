{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
module Term
( Term
, TermF
, SyntaxTerm
, SyntaxTermF
, zipTerms
, termSize
, alignCofreeWith
, cofree
, runCofree
, Cofree(..)
, extract
, unwrap
, hoistCofree
, CofreeF.headF
, CofreeF.tailF
, CofreeF.CofreeF()
) where

import Control.Comonad
import Control.Comonad.Cofree.Class
import qualified Control.Comonad.Trans.Cofree as CofreeF
import Control.DeepSeq
import Control.Monad.Free
import Data.Align.Generic
import Data.Functor.Both
import Data.Functor.Classes.Pretty.Generic
import Data.Functor.Foldable
import Data.Maybe
import Data.Proxy
import Data.Record
import Data.These
import Data.Union
import Syntax

-- | A Term with an abstract syntax tree and an annotation.
type Term f = Cofree f
type TermF = CofreeF.CofreeF

infixr 5 :<
data Cofree f a = a :< f (Cofree f a)

-- | A Term with a Syntax leaf and a record of fields.
type SyntaxTerm fields = Term Syntax (Record fields)
type SyntaxTermF fields = TermF Syntax (Record fields)

instance (NFData (f (Cofree f a)), NFData a, Functor f) => NFData (Cofree f a) where
  rnf = rnf . runCofree

instance (NFData a, NFData (f b)) => NFData (CofreeF.CofreeF f a b) where
  rnf (a CofreeF.:< s) = rnf a `seq` rnf s `seq` ()

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: (Traversable f, GAlign f) => Term f annotation -> Term f annotation -> Maybe (Term f (Both annotation))
zipTerms t1 t2 = iter go (alignCofreeWith galign (const Nothing) both (These t1 t2))
  where go (a CofreeF.:< s) = cofree . (a CofreeF.:<) <$> sequenceA s

-- | Return the node count of a term.
termSize :: (Foldable f, Functor f) => Term f annotation -> Int
termSize = cata size where
  size (_ CofreeF.:< syntax) = 1 + sum syntax

-- | Aligns (zips, retaining non-overlapping portions of the structure) a pair of terms.
alignCofreeWith :: Functor f
  => (forall a b. f a -> f b -> Maybe (f (These a b))) -- ^ A function comparing a pair of structures, returning `Just` the combined structure if they are comparable (e.g. if they have the same constructor), and `Nothing` otherwise. The 'Data.Align.Generic.galign' function is usually what you want here.
  -> (These (Term f a) (Term f b) -> contrasted) -- ^ A function mapping a 'These' of incomparable terms into 'Pure' values in the resulting tree.
  -> (a -> b -> combined) -- ^ A function mapping the input terms’ annotations into annotations in the 'Free' values in the resulting tree.
  -> These (Term f a) (Term f b) -- ^ The input terms.
  -> Free (TermF f combined) contrasted
alignCofreeWith compare contrast combine = go
  where go terms = fromMaybe (pure (contrast terms)) $ case terms of
          These (a1 :< f1) (a2 :< f2) -> wrap . (combine a1 a2 CofreeF.:<) . fmap go <$> compare f1 f2
          _ -> Nothing


cofree :: CofreeF.CofreeF f a (Cofree f a) -> Cofree f a
cofree (a CofreeF.:< f) = a :< f

runCofree :: Cofree f a -> CofreeF.CofreeF f a (Cofree f a)
runCofree (a :< f) = a CofreeF.:< f

hoistCofree :: Functor f => (forall a. f a -> g a) -> Cofree f a -> Cofree g a
hoistCofree f = go where go (a :< r) = a :< f (fmap go r)

instance Pretty1 f => Pretty1 (Cofree f) where
  liftPretty p pl = go where go (a :< f) = p a <+> liftPretty go (list . map (liftPretty p pl)) f

instance (Pretty1 f, Pretty a) => Pretty (Cofree f a) where
  pretty = liftPretty pretty prettyList

instance Apply1 Pretty1 fs => Pretty1 (Union fs) where
  liftPretty p pl = apply1 (Proxy :: Proxy Pretty1) (liftPretty p pl)

type instance Base (Cofree f a) = CofreeF.CofreeF f a

instance Functor f => Recursive (Cofree f a) where project = runCofree
instance Functor f => Corecursive (Cofree f a) where embed = cofree

instance Functor f => Comonad (Cofree f) where
  extract (a :< _) = a
  duplicate w = w :< fmap duplicate (unwrap w)
  extend f = go where go w = f w :< fmap go (unwrap w)

instance Functor f => Functor (Cofree f) where
  fmap f = go where go (a :< r) = f a :< fmap go r

instance Functor f => ComonadCofree f (Cofree f) where
  unwrap (_ :< as) = as
  {-# INLINE unwrap #-}

instance (Eq (f (Cofree f a)), Eq a) => Eq (Cofree f a) where
  a1 :< f1 == a2 :< f2 = a1 == a2 && f1 == f2

instance (Show (f (Cofree f a)), Show a) => Show (Cofree f a) where
  showsPrec d (a :< f) = showParen (d > 5) $ showsPrec 6 a . showString " :< " . showsPrec 5 f
