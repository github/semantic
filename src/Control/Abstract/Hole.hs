module Control.Abstract.Hole
  ( AbstractHole (..)
  , Hole (..)
  , toMaybe
  ) where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Prologue

class AbstractHole a where
  hole :: a


data Hole context a = Partial context | Total a
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

instance Lower context => AbstractHole (Hole context a) where
  hole = Partial lowerBound

toMaybe :: Hole context a -> Maybe a
toMaybe (Partial _) = Nothing
toMaybe (Total a)   = Just a


instance (Allocatable address effects, Ord context, Show context) => Allocatable (Hole context address) effects where
  allocCell name = relocate (Total <$> allocCell name)

instance (Derefable address effects, Ord context, Show context) => Derefable (Hole context address) effects where
  derefCell (Total loc) = relocate . derefCell loc
  derefCell (Partial _) = const (pure Nothing)

  assignCell (Total loc) value = relocate . assignCell loc value
  assignCell (Partial _) _ = pure

relocate :: Evaluator address1 value effects a -> Evaluator address2 value effects a
relocate = raiseEff . lowerEff
