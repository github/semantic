module Data.Sequenceable where

import Prologue

class Sequenceable t where
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
