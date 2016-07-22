module Data.Sequenceable.Generic where

import Prologue

class GSequenceable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)
