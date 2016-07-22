module Data.Sequenceable where

import Prologue

-- Classes

class Sequenceable t where
  sequenceAlt :: Alternative f => t (f a) -> f (t a)


-- Instances

instance Sequenceable [] where
  sequenceAlt (x:xs) = ((:) <$> x <|> pure identity) <*> sequenceAlt xs
  sequenceAlt [] = pure []
