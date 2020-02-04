{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeApplications, TypeOperators, UndecidableInstances #-}
module Data.Mergeable ( Mergeable (..) ) where

import Control.Applicative
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Sum
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import GHC.Generics

-- Classes

-- | A 'Mergeable' functor is one which supports pushing itself through an 'Alternative' functor. Note the similarities with 'Traversable' & 'Crosswalk'.
--
-- This is a kind of distributive law which produces (at least) the union of the two functors’ shapes; i.e. unlike 'Traversable', an 'empty' value in the inner functor does not produce an 'empty' result, and unlike 'Crosswalk', an 'empty' value in the outer functor does not produce an 'empty' result.
--
-- For example, 'Data.Diff' uses 'sequenceAlt' to select one side or the other of a diff node, while correctly handling the fact that some patches don’t have any content for that side.
class Functor t => Mergeable t where
  -- | Sequence a 'Mergeable' functor by merging the 'Alternative' values.
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  default sequenceAlt :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => t (f a) -> f (t a)
  sequenceAlt = genericSequenceAlt


-- Instances

instance Mergeable [] where
  sequenceAlt = foldr (\ x -> (((:) <$> x <|> pure id) <*>)) (pure [])

instance Mergeable NonEmpty where
  sequenceAlt (x :|[])    = (:|) <$> x  <*> pure []
  sequenceAlt (x1:|x2:xs) = (:|) <$> x1 <*> sequenceAlt (x2 : xs) <|> sequenceAlt (x2:|xs)

instance Mergeable Maybe where
  sequenceAlt = maybe (pure empty) (fmap Just)

instance Mergeable Identity where
  sequenceAlt = fmap Identity . runIdentity

instance (Apply Functor fs, Apply Mergeable fs) => Mergeable (Sum fs) where
  sequenceAlt = apply' @Mergeable (\ reinj t -> reinj <$> sequenceAlt t)

instance Mergeable Comment.Comment
instance Mergeable Declaration.Function
instance Mergeable Declaration.Method
instance Mergeable Statement.If
instance Mergeable Syntax.Context
instance Mergeable Syntax.Empty
instance Mergeable Syntax.Identifier


-- Generics

class GMergeable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)

genericSequenceAlt :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => t (f a) -> f (t a)
genericSequenceAlt = fmap to1 . gsequenceAlt . from1


-- Instances

instance GMergeable U1 where
  gsequenceAlt _ = pure U1

instance GMergeable Par1 where
  gsequenceAlt (Par1 a) = Par1 <$> a

instance GMergeable (K1 i c) where
  gsequenceAlt (K1 a) = pure (K1 a)

instance Mergeable f => GMergeable (Rec1 f) where
  gsequenceAlt (Rec1 a) = Rec1 <$> sequenceAlt a

instance GMergeable f => GMergeable (M1 i c f) where
  gsequenceAlt (M1 a) = M1 <$> gsequenceAlt a

instance (GMergeable f, GMergeable g) => GMergeable (f :+: g) where
  gsequenceAlt (L1 a) = L1 <$> gsequenceAlt a
  gsequenceAlt (R1 a) = R1 <$> gsequenceAlt a

instance (GMergeable f, GMergeable g) => GMergeable (f :*: g) where
  gsequenceAlt (a :*: b) = (:*:) <$> gsequenceAlt a <*> gsequenceAlt b
