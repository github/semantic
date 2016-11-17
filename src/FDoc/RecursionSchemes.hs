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

{-
Catamorphism

cata :: (Base t a -> a) -- ^ a (Base t)-algebra
       -> t               -- ^ fixed point
       -> a               -- ^ result

Catamorphism as a recursion scheme "tears down" a recursive structure.
Catamorphisms work by using an algebra, which maps a shape in our fixed point structure to a new shape.

The example below adds a new field to the `Record` fields.
-}
indexedTermCata :: [leaf] -> Term (Syntax leaf) (Record '[NewField, Range, Category])
indexedTermCata childrenLeaves = cata algebra (indexedTerm childrenLeaves)
  where
    algebra term = cofree $ (NewField .: (headF term)) :< (tailF term)
