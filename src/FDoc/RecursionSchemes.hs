{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module FDoc.RecursionSchemes where

import Data.Record
import Range
import Category
import Term
import Syntax
import Prologue
import Prelude
import Data.Functor.Foldable hiding (ListF)
import FDoc.Term

data NewField = NewField deriving (Show)

{-
Anamorphism

ana :: (a -> Base t a) -- ^ a (Base t)-coalgebra
    -> a               -- ^ seed
    -> t               -- ^ resulting fixed point

Anamorphism as a recursion scheme "builds up" a recursive structure.
Anamorphisms work by using a coalgebra, which maps a seed value to a fixed point structure.

The example below adds a new field to the `Record` fields.

-}

indexedTermAna :: [leaf] -> Term (Syntax leaf) (Record '[NewField, Range, Category])
indexedTermAna childrenLeaves = ana coalgebra (indexedTerm childrenLeaves)
  where
    coalgebra term = (NewField .: (extract term)) :< (unwrap term)
