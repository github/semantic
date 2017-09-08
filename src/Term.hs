{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Term
( Term(..)
, TermF(..)
, SyntaxTerm
, SyntaxTermF
, termSize
, term
, unTerm
, extract
, unwrap
, hoistTerm
) where

import Control.Comonad
import Control.Comonad.Cofree.Class
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Classes.Pretty.Generic as Pretty
import Data.Functor.Foldable
import Data.Functor.Listable
import Data.Proxy
import Data.Record
import Data.Union
import Syntax

-- | A Term with an abstract syntax tree and an annotation.
infixr 5 :<
data Term syntax ann = ann :< syntax (Term syntax ann)
infixr 5 :<<
data TermF syntax ann recur = (:<<) { headF :: ann, tailF :: syntax recur }
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | A Term with a Syntax leaf and a record of fields.
type SyntaxTerm fields = Term Syntax (Record fields)
type SyntaxTermF fields = TermF Syntax (Record fields)

-- | Return the node count of a term.
termSize :: (Foldable f, Functor f) => Term f annotation -> Int
termSize = cata size where
  size (_ :<< syntax) = 1 + sum syntax


term :: TermF f a (Term f a) -> Term f a
term (a :<< f) = a :< f

unTerm :: Term f a -> TermF f a (Term f a)
unTerm (a :< f) = a :<< f

hoistTerm :: Functor f => (forall a. f a -> g a) -> Term f a -> Term g a
hoistTerm f = go where go (a :< r) = a :< f (fmap go r)

liftPrettyUnion :: Apply1 Pretty1 fs => (a -> Doc ann) -> ([a] -> Doc ann) -> Union fs a -> Doc ann
liftPrettyUnion p pl = apply1 (Proxy :: Proxy Pretty1) (liftPretty p pl)

instance Apply1 Pretty1 fs => Pretty1 (Term (Union fs)) where
  liftPretty p pl = go where go (a :< f) = p a <+> liftPrettyUnion go (Pretty.list . map (liftPretty p pl)) f

instance (Apply1 Pretty1 fs, Pretty a) => Pretty (Term (Union fs) a) where
  pretty = liftPretty pretty prettyList

type instance Base (Term f a) = TermF f a

instance Functor f => Recursive (Term f a) where project = unTerm
instance Functor f => Corecursive (Term f a) where embed = term

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
    where go = liftCons1 (liftTiers2 annotationTiers go) term

instance Eq1 f => Eq2 (TermF f) where
  liftEq2 eqA eqB (a1 :<< f1) (a2 :<< f2) = eqA a1 a2 && liftEq eqB f1 f2

instance (Eq1 f, Eq a) => Eq1 (TermF f a) where
  liftEq = liftEq2 (==)

instance Show1 f => Show2 (TermF f) where
  liftShowsPrec2 spA _ spB slB d (a :<< f) = showParen (d > 5) $ spA 6 a . showString " :<< " . liftShowsPrec spB slB 5 f

instance (Show1 f, Show a) => Show1 (TermF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Pretty1 f => Pretty2 (TermF f) where
  liftPretty2 pA _ pB plB (a :<< f) = pA a <+> liftPretty pB plB f

instance (Pretty1 f, Pretty a) => Pretty1 (TermF f a) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty1 f, Pretty a, Pretty b) => Pretty (TermF f a b) where
  pretty = liftPretty pretty prettyList
