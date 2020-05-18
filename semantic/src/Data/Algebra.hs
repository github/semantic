{-# LANGUAGE DeriveGeneric, DeriveTraversable, RankNTypes #-}
module Data.Algebra
  ( FAlgebra
  , RAlgebra
  , Subterm(..)
  , SubtermAlgebra
  , embedTerm
  , foldSubterms
  ) where

import Data.Functor.Classes.Generic as X
import Data.Functor.Foldable ( Base
                             , Corecursive(embed)
                             , Recursive(project)
                             )
import GHC.Generics

-- | An F-algebra on some 'Recursive' type @t@.
--
--   An F-algebra is an evaluator for a functor (@f@) populated with the results (@a@s) of evaluating each child of the functor to a result (@a@), applied at each node starting from the leaves and working towards the root. @a@ is called the carrier type of the algebra because it carries the (results of the) functions used to compute the functor’s structure and enforce the laws of that algebra.
type FAlgebra f a = f a -> a

-- | An R-algebra on some 'Recursive' type @t@.
--
--   An R-algebra is an evaluator for a functor (@f@) populated with pairs of the children (@t@) and the results (@a@s) of evaluating each child of the functor to a result (@a@), applied at each node starting from the leaves and working towards the root.
--
--   See also 'FAlgebra'.
type RAlgebra f t a = f (t, a) -> a

-- | A subterm and its computed value, used in 'SubtermAlgebra'.
data Subterm t a = Subterm { subterm :: !t, subtermRef :: !a }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq t => Eq1 (Subterm t) where liftEq = genericLiftEq
instance Ord t => Ord1 (Subterm t) where liftCompare = genericLiftCompare
instance Show t => Show1 (Subterm t) where liftShowsPrec = genericLiftShowsPrec

-- | Like an R-algebra, but using 'Subterm' to label the fields instead of an anonymous pair.
type SubtermAlgebra f t a = f (Subterm t a) -> a


-- | Fold a 'Recursive' structure using a 'SubtermAlgebra'. Like 'para', but with named fields for subterms and values.
foldSubterms :: Recursive t => SubtermAlgebra (Base t) t a -> t -> a
foldSubterms algebra = go where go = algebra . fmap (Subterm <*> go) . project

-- | Extract a term from the carrier tuple associated with a paramorphism. See also 'embedSubterm'.
embedTerm :: Corecursive t => Base t (t, a) -> t
embedTerm e = embed (fst <$> e)
