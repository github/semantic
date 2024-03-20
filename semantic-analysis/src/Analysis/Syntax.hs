{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Syntax
( -- * Syntax
  Term(..)
, subterms
, foldTerm
, paraTerm
, mendlerTerm
, mendlerParaTerm
) where

import qualified Data.Set as Set

-- Syntax

-- | (Currently) untyped term representations.
data Term sig v
  = Var v
  | Term (sig (Term sig v))

instance (Eq (sig (Term sig v)), Eq v) => Eq (Term sig v) where
  Var v1  == Var v2  = v1 == v2
  Term s1 == Term s2 = s1 == s2
  _       == _       = False

instance (Ord (sig (Term sig v)), Ord v) => Ord (Term sig v) where
  compare (Var v1)  (Var v2)  = compare v1 v2
  compare (Var _)   _         = LT
  compare (Term s1) (Term s2) = compare s1 s2
  compare _          _        = GT


subterms :: (Ord (sig (Term sig v)), Ord v, Foldable sig) => Term sig v -> Set.Set (Term sig v)
subterms = mendlerParaTerm (Set.singleton . Var) (\ k -> foldMap (uncurry Set.insert . k))

foldTerm :: Functor sig => (v -> r) -> (sig r -> r) -> (Term sig v -> r)
foldTerm var sig = mendlerTerm var (\ k -> sig . fmap k)

paraTerm :: Functor sig => (v -> r) -> (sig (Term sig v, r) -> r) -> (Term sig v -> r)
paraTerm var sig = mendlerParaTerm var (\ k -> sig . fmap k)

mendlerTerm :: (v -> r) -> (forall r' . (r' -> r) -> sig r'-> r) -> (Term sig v -> r)
mendlerTerm var sig = go
  where
  go (Var v)  = var v
  go (Term s) = sig go s

mendlerParaTerm :: (v -> r) -> (forall r' . (r' -> (Term sig v, r)) -> sig r'-> r) -> (Term sig v -> r)
mendlerParaTerm var sig = go
  where
  go (Var v)  = var v
  go (Term s) = sig ((,) <*> go) s
