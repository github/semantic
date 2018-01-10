{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators #-}
module Data.Term
( Term(..)
, termIn
, termAnnotation
, termOut
, TermF(..)
, termSize
, hoistTerm
, hoistTermF
, stripTerm
) where

import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.JSON.Fields
import Data.Record
import Data.Semigroup ((<>))
import Text.Show

-- | A Term with an abstract syntax tree and an annotation.
newtype Term syntax ann = Term { unTerm :: TermF syntax ann (Term syntax ann) }

termAnnotation :: Term syntax ann -> ann
termAnnotation = termFAnnotation . unTerm

termOut :: Term syntax ann -> syntax (Term syntax ann)
termOut = termFOut . unTerm


data TermF syntax ann recur = In { termFAnnotation :: ann, termFOut :: syntax recur }
  deriving (Eq, Ord, Foldable, Functor, Show, Traversable)


-- | Return the node count of a term.
termSize :: (Foldable f, Functor f) => Term f annotation -> Int
termSize = cata size where
  size (In _ syntax) = 1 + sum syntax

-- | Build a Term from its annotation and syntax.
termIn :: ann -> syntax (Term syntax ann) -> Term syntax ann
termIn = (Term .) . In


hoistTerm :: Functor f => (forall a. f a -> g a) -> Term f a -> Term g a
hoistTerm f = go where go (Term r) = Term (hoistTermF f (fmap go r))

hoistTermF :: (forall a. f a -> g a) -> TermF f a b -> TermF g a b
hoistTermF f = go where go (In a r) = In a (f r)

-- | Strips the head annotation off a term annotated with non-empty records.
stripTerm :: Functor f => Term f (Record (h ': t)) -> Term f (Record t)
stripTerm = fmap rtail


type instance Base (Term f a) = TermF f a

instance Functor f => Recursive (Term f a) where project = unTerm
instance Functor f => Corecursive (Term f a) where embed = Term

instance Functor f => Functor (Term f) where
  fmap f = go where go = Term . bimap f go . unTerm

instance Foldable f => Foldable (Term f) where
  foldMap f = go where go = bifoldMap f go . unTerm

instance Traversable f => Traversable (Term f) where
  traverse f = go where go = fmap Term . bitraverse f go . unTerm

instance Eq1 f => Eq1 (Term f) where
  liftEq eqA = go where go t1 t2 = liftEq2 eqA go (unTerm t1) (unTerm t2)

instance (Eq1 f, Eq a) => Eq (Term f a) where
  (==) = eq1

instance Show1 f => Show1 (Term f) where
  liftShowsPrec spA slA = go where go d = showsUnaryWith (liftShowsPrec2 spA slA go (showListWith (go 0))) "Term" d . unTerm

instance (Show1 f, Show a) => Show (Term f a) where
  showsPrec = showsPrec1

instance Ord1 f => Ord1 (Term f) where
  liftCompare comp = go where go t1 t2 = liftCompare2 comp go (unTerm t1) (unTerm t2)

instance (Ord1 f, Ord a) => Ord (Term f a) where
  compare = compare1


instance Functor f => Bifunctor (TermF f) where
  bimap f g (In a r) = In (f a) (fmap g r)

instance Foldable f => Bifoldable (TermF f) where
  bifoldMap f g (In a r) = f a `mappend` foldMap g r

instance Traversable f => Bitraversable (TermF f) where
  bitraverse f g (In a r) = In <$> f a <*> traverse g r


instance Eq1 f => Eq2 (TermF f) where
  liftEq2 eqA eqB (In a1 f1) (In a2 f2) = eqA a1 a2 && liftEq eqB f1 f2

instance (Eq1 f, Eq a) => Eq1 (TermF f a) where
  liftEq = liftEq2 (==)

instance Show1 f => Show2 (TermF f) where
  liftShowsPrec2 spA _ spB slB d (In a f) = showsBinaryWith spA (liftShowsPrec spB slB) "In" d a f

instance (Show1 f, Show a) => Show1 (TermF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Ord1 f => Ord2 (TermF f) where
  liftCompare2 compA compB (In a1 f1) (In a2 f2) = compA a1 a2 <> liftCompare compB f1 f2

instance (Ord1 f, Ord a) => Ord1 (TermF f a) where
  liftCompare = liftCompare2 compare

instance (ToJSONFields a, ToJSONFields1 f) => ToJSON (Term f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields (Term f a) where
  toJSONFields = toJSONFields . unTerm

instance (ToJSON b, ToJSONFields a, ToJSONFields1 f) => ToJSONFields (TermF f a b) where
  toJSONFields (In a f) = toJSONFields a <> toJSONFields1 f

instance (ToJSON b, ToJSONFields a, ToJSONFields1 f) => ToJSON (TermF f a b) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields
