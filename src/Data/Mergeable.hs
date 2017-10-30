{-# LANGUAGE DefaultSignatures, TypeOperators, UndecidableInstances #-}
module Data.Mergeable where

import Control.Applicative
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Proxy
import Data.Union
import GHC.Generics

-- Classes

-- | A 'Mergeable' functor is one which supports pushing itself through an 'Alternative' functor. Note the similarities with 'Traversable' & 'Crosswalk'.
--
-- This is a kind of distributive law which produces (at least) the union of the two functors’ shapes; i.e. unlike 'Traversable', an 'empty' value in the inner functor does not produce an 'empty' result, and unlike 'Crosswalk', an 'empty' value in the outer functor does not produce an 'empty' result.
--
-- For example, we can use 'merge' to select one side or the other of a diff node in 'Syntax', while correctly handling the fact that some patches don’t have any content for that side:
--
-- @
-- let before = iter (\ (a :< s) -> cofree . (fst a :<) <$> sequenceAlt syntax) . fmap (maybeFst . unPatch)
-- @
class Functor t => Mergeable t where
  -- | Merge a functor by mapping its elements into an 'Alternative' functor, combining them, and pushing the 'Mergeable' functor inside.
  merge :: Alternative f => (a -> f b) -> t a -> f (t b)
  default merge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
  merge = genericMerge

  -- | Sequnce a 'Mergeable' functor by 'merge'ing the 'Alternative' values.
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  sequenceAlt = merge id


-- Instances

instance Mergeable [] where
  merge f (x:xs) = ((:) <$> f x <|> pure id) <*> merge f xs
  merge _ [] = pure []

instance Mergeable NonEmpty where
  merge f (x:|[]) = (:|) <$> f x <*> pure []
  merge f (x1:|x2:xs) = (:|) <$> f x1 <*> merge f (x2 : xs) <|> merge f (x2:|xs)

instance Mergeable Maybe where
  merge f (Just a) = Just <$> f a
  merge _ Nothing = pure empty

instance Mergeable Identity where merge f = fmap Identity . f . runIdentity

instance (Apply Functor fs, Apply Mergeable fs) => Mergeable (Union fs) where
  merge f u = apply' (Proxy :: Proxy Mergeable) (\ reinj g -> reinj <$> merge f g) u


-- Generics

class GMergeable t where
  gmerge :: Alternative f => (a -> f b) -> t a -> f (t b)

genericMerge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
genericMerge f = fmap to1 . gmerge f . from1


-- Instances

instance GMergeable U1 where
  gmerge _ _ = pure U1

instance GMergeable Par1 where
  gmerge f (Par1 a) = Par1 <$> f a

instance GMergeable (K1 i c) where
  gmerge _ (K1 a) = pure (K1 a)

instance Mergeable f => GMergeable (Rec1 f) where
  gmerge f (Rec1 a) = Rec1 <$> merge f a

instance GMergeable f => GMergeable (M1 i c f) where
  gmerge f (M1 a) = M1 <$> gmerge f a

instance (GMergeable f, GMergeable g) => GMergeable (f :+: g) where
  gmerge f (L1 a) = L1 <$> gmerge f a
  gmerge f (R1 b) = R1 <$> gmerge f b

instance (GMergeable f, GMergeable g) => GMergeable (f :*: g) where
  gmerge f (a :*: b) = (:*:) <$> gmerge f a <*> gmerge f b
