{-# LANGUAGE RankNTypes, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
, CofreeF.CofreeF(..)
) where

import qualified Control.Comonad.Cofree as Cofree
import qualified Control.Comonad.Trans.Cofree as CofreeF
import Control.DeepSeq
import Control.Monad.Free
import Data.Align.Generic
import Data.Functor.Both
import Data.Functor.Foldable
import Data.Maybe
import Data.Record
import Data.These
import Syntax

-- | A Term with an abstract syntax tree and an annotation.
type Term f = Cofree.Cofree f
type TermF = CofreeF.CofreeF

-- | A Term with a Syntax leaf and a record of fields.
type SyntaxTerm fields = Term Syntax (Record fields)
type SyntaxTermF fields = TermF Syntax (Record fields)

instance (NFData (f (Cofree.Cofree f a)), NFData a, Functor f) => NFData (Cofree.Cofree f a) where
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
          These (a1 Cofree.:< f1) (a2 Cofree.:< f2) -> wrap . (combine a1 a2 CofreeF.:<) . fmap go <$> compare f1 f2
          _ -> Nothing


cofree :: CofreeF.CofreeF f a (Cofree.Cofree f a) -> Cofree.Cofree f a
cofree (a CofreeF.:< f) = a Cofree.:< f

runCofree :: Cofree.Cofree f a -> CofreeF.CofreeF f a (Cofree.Cofree f a)
runCofree (a Cofree.:< f) = a CofreeF.:< f
