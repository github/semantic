{-# LANGUAGE QuantifiedConstraints #-}
module Analysis.Syntax
( -- * Syntax
  Term(..)
, subterms
) where

import qualified Data.Set as Set

-- Syntax

-- | (Currently) untyped term representations.
data Term sig v
  = Var v
  | Term (sig (Term sig v))

instance (forall t . Eq t => Eq (sig t), Eq v) => Eq (Term sig v) where
  Var v1   == Var v2 = v1 == v2
  Term s1 == Term s2 = s1 == s2
  _        == _      = False

instance (forall t . Eq t => Eq (sig t), forall t. Ord t => Ord (sig t), Ord v) => Ord (Term sig v) where
  compare (Var v1)  (Var v2)  = compare v1 v2
  compare (Var _)   _         = LT
  compare (Term s1) (Term s2) = compare s1 s2
  compare _          _        = GT


subterms :: (forall t . Eq t => Eq (sig t), forall t . Ord t => Ord (sig t), Ord v, Foldable sig) => Term sig v -> Set.Set (Term sig v)
subterms t = case t of
  Var _  -> Set.singleton t
  Term s -> Set.insert t (foldMap subterms s)
