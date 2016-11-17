{-# LANGUAGE DataKinds, TypeOperators #-}
module FDoc.Term where

import Data.Record
import Range
import Category
import Term
import Syntax
import Prologue

{-

Constructs a Syntax.Leaf using the polymorphic type variable `leaf`.

This is in the TermF shape: CofreeF f a b where
  f is the functor (Syntax.Leaf `leaf`)
  a is the annotation (Record '[Range, Category])
  b is the hidden functor

Two common convenience operations when working with CofreeF (for docs, see Control.Comonad.Trans.Cofree.Types.CofreeF) are `headF` and `tailF`. `headF` return the annotation portion of the CofreeF structure, and `tailF` returns the functor portion (Syntax).

Example (from GHCi):

> let leaf = leafTermF "example"
> headF leaf
> Range {start = 1, end = 10} .: MethodCall .: RNil
> tailF leaf
> Leaf "example"

-}

leafTermF :: leaf -> TermF (Syntax leaf) (Record '[Range, Category]) b
leafTermF leaf = (Range 1 10 .: Category.MethodCall .: RNil) :< Leaf leaf
