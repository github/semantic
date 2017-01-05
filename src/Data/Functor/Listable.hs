module Data.Functor.Listable
( Listable(..)
, cons0
, cons1
, cons2
, cons3
, cons4
, cons5
, cons6
, (\/)
, Listable1(..)
) where

import Test.LeanCheck

class Listable1 l where
  liftTiers :: [[a]] -> [[l a]]
