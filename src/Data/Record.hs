{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
module Data.Record where

import Prologue

infix 9 :=>

newtype a :=> b = (:=>) b
  deriving (Eq, Show)

field :: b -> a :=> b
field = (:=>)
