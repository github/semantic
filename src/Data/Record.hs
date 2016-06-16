{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.Record where

import Data.Tagged
import Prologue

infix 9 :=>

type a :=> b = Tagged a b

field :: b -> a :=> b
field = Tagged

data Record :: [*] -> * where
  RNil :: Record '[]
  RCons :: h -> Record t -> Record (h ': t)
