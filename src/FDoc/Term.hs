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
  b is the same type of functor defined by f

Two common convenience operations when working with CofreeF (for docs, see
Control.Comonad.Trans.Cofree.Types.CofreeF) are `headF` and `tailF`. `headF`
return the annotation portion of the CofreeF structure, and `tailF` returns the
functor portion (Syntax).

Example (from GHCi):

> let leaf = leafTermF "example"
> headF leaf
> Range {start = 1, end = 10} :. MethodCall :. Nil
> tailF leaf
> Leaf "example"

-}

leafTermF :: leaf -> TermF (Syntax leaf) (Record '[Range, Category]) b
leafTermF leaf = (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf leaf

{-

Constructs a Syntax.Leaf using the polymorphic type variable `leaf`.

This is in the Term shape: Cofree f a where
  f is the functor (Syntax.Leaf `leaf`)
  a is the annotation (Record '[Range, Category])

Two common convenience operations when working with Cofree (for docs, see
Control.Comonad.Trans.Cofree.Types.Cofree) are `extract` and `unwrap`. `extract`
returns the annotation portion of the Cofree structure, and `unwrap` returns the
functor portion (Syntax).

Example (from GHCi):

> let leaf = leafTerm "example"
> extract leaf
> Range {start = 1, end = 10} :. MethodCall :. Nil
> unwrap leaf
> Leaf "example"

-}
leafTerm :: leaf -> Cofree (Syntax leaf) (Record '[Range, Category])
leafTerm = cofree . leafTermF

indexedTermF :: [leaf] -> TermF (Syntax leaf) (Record '[Range, Category]) (Term (Syntax leaf) (Record '[Range, Category]))
indexedTermF leaves = (Range 1 10 :. Category.MethodCall :. Nil) :< Indexed (leafTerm <$> leaves)

indexedTerm :: [leaf] -> Term (Syntax leaf) (Record '[Range, Category])
indexedTerm leaves = cofree $ indexedTermF leaves
