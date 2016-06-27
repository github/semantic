{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Diff where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both as Both
import qualified Data.OrderedMap as Map
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF leaf annotation = FreeF (CofreeF (Syntax leaf) (Both annotation)) (Patch (Term leaf annotation))
type Diff a annotation = Free (CofreeF (Syntax a) (Both annotation)) (Patch (Term a annotation))

type instance Base (Free f a) = FreeF f a
instance (Functor f) => Foldable.Foldable (Free f a) where project = runFree
instance (Functor f) => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize

beforeTerm :: Diff leaf annotation -> Maybe (Term leaf annotation)
beforeTerm = cata algebra
  where algebra :: FreeF (CofreeF (Syntax leaf) (Both annotation)) (Patch (Term leaf annotation)) (Maybe (Term leaf annotation)) -> Maybe (Term leaf annotation)
        algebra (Pure patch) = before patch
        algebra (Free (annotations :< syntax)) = Just . cofree $ Both.fst annotations :< case syntax of
          Leaf s -> Leaf s
          Indexed i -> Indexed (catMaybes i)
          Fixed i -> Fixed (catMaybes i)
          Keyed i -> Keyed (Map.fromList (Map.toList i >>= (\ (k, v) -> maybe [] (pure . (,) k) v)))
