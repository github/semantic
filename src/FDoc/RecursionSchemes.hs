{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module FDoc.RecursionSchemes where

import Data.Range
import Data.Record
import Category
import Term
import Prologue
import Prelude
import Syntax
import FDoc.Term

data NewField = NewField deriving (Show)

{-
Anamorphism -- add a new field to each term's Record fields

ana :: (a -> Base t a) -- a (Base t)-coalgebra
    -> a               -- seed
    -> t               -- resulting fixed point

Anamorphism as a recursion scheme "builds up" a recursive structure.
Anamorphisms work by using a coalgebra, which maps a seed value to a fixed point
structure.

The example below adds a new field to the `Record` fields.
-}
indexedTermAna :: [Text] -> Term Syntax (Record '[NewField, Range, Category])
indexedTermAna childrenLeaves = ana coalgebra (indexedTerm childrenLeaves)
  where
    coalgebra term = (NewField :. (extract term)) :< unwrap term

{-
Catamorphism example -- add a new field to each term's Record fields

cata :: (Base t a -> a) -- a (Base t)-algebra
       -> t             -- fixed point
       -> a             -- result

Catamorphism as a recursion scheme "tears down" a recursive structure.
Catamorphisms work by using an algebra, which maps a shape in our fixed point
structure to a new shape.

The example below adds a new field to the `Record` fields.
-}
indexedTermCata :: [Text] -> Term Syntax (Record '[NewField, Range, Category])
indexedTermCata childrenLeaves = cata algebra (indexedTerm childrenLeaves)
  where
    algebra :: Functor f => CofreeF f (Record t) (Cofree f (Record (NewField : t))) -> Cofree f (Record (NewField : t))
    algebra term = cofree $ (NewField :. headF term) :< tailF term

{-
Anamorphism -- construct a Term from a string

The example below shows how to build up a recursive Term structure from a string
representation.

Example usage:

stringToTermAna "indexed" =>
  CofreeT (Identity ( (Range {start = 1, end = 10} :. MethodCall :. Nil)
                      :<
                      Indexed
                        [ CofreeT (Identity ( (Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf1" ) )
                        , CofreeT (Identity ( (Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf2" ) )
                        , CofreeT (Identity ( (Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf3" ) )
                        ] ))

  First step is to match against the "indexed" string and begin building up a Cofree Indexed structure:

  CofreeT (Identity ( (Range 1 10 :. Category.MethodCall :. Nil) :< Indexed ["leaf1", "leaf2", "leaf3"] ) )

  While building up the `Indexed` structure, we continue to recurse over the
  `Indexed` terms ["leaf1", "leaf2", "leaf3"]. These are pattern matched using
  the catch all `_` and default to `Leaf` Syntax shapes:

  CofreeT (Identity ( (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf "leaf1" ) )
  CofreeT (Identity ( (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf "leaf2" ) )
  CofreeT (Identity ( (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf "leaf3" ) )

  These structures are substituted in place of ["leaf1", "leaf2", "leaf3"] in
  the new cofree `Indexed` structure, resulting in a expansion of all possible
  string terms.
-}
stringToTermAna :: Text -> Term Syntax (Record '[Range, Category])
stringToTermAna = ana coalgebra
  where
    coalgebra representation = case representation of
      "indexed" -> (Range 1 10 :. Category.MethodCall :. Nil) :< Indexed ["leaf1", "leaf2", "leaf3"]
      _ -> (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf representation

{-
Catamorphism -- construct a list of Strings from a recursive Term structure.

The example below shows how to tear down a recursive Term structure into a list
of String representation.
-}
termToStringCata :: Term Syntax (Record '[Range, Category]) -> [Text]
termToStringCata = cata algebra
  where
    algebra term = case term of
      (_ :< Leaf value) -> [value]
      (_ :< Indexed values) -> ["indexed"] <> Prologue.concat values
      _ -> ["unknown"]

{-
Hylomorphism -- An anamorphism followed by a catamorphism

hylo :: Functor f => (f b -> b) -- an algebra
                  -> (a -> f a) -- a coalgebra
                  -> a          -- seed value
                  -> b          -- result

Hylomorphisms work by first applying a coalgebra (anamorphism) to build up a
structure. An algebra (catamorphism) is then applied to this structure. Because
of fusion the anamorphism and catamorphism occur in a single pass rather than
two separate traversals.

The example below shows how our algebra and coalgebra defined in the
termToStringCata and stringToTermAna can be utilized as a hylomorphism.

Example Usage:
stringTermHylo "indexed" => ["indexed", "leaf1", "leaf2", "leaf3"]

-}
stringTermHylo :: Text -> [Text]
stringTermHylo = hylo algebra coalgebra
  where
    algebra term = case term of
      (_ :< Leaf value) -> [value]
      (_ :< Indexed values) -> ["indexed"] <> Prologue.concat values
      _ -> ["unknown"]
    coalgebra stringRepresentation = case stringRepresentation of
      "indexed" -> (Range 1 10 :. Category.MethodCall :. Nil) :< Indexed ["leaf1", "leaf2", "leaf3"]
      _ -> (Range 1 10 :. Category.MethodCall :. Nil) :< Leaf stringRepresentation

{-
Paramorphism -- primitive recursion that maintains a reference to the original value and its computed value.

para :: (Base t (t, a) -> a) -- an algebra that takes a tuple of the last input
       -> t                  -- fixed point
       -> a                  -- result

Paramorphisms, like all recursion schemes, work via a bottom up traversal
(leaves to root), in which an algebra is applied to every node in the recursive
structure. The difference between paramorphisms and catamorphisms is the algebra
receives a tuple of the original subobject and its computed value (t, a) where
`t` is the original suboject and `a` is the computed value.

The example implementation below calculates a string representation for each
Syntax type, flattening the recursive structure into a one dimensional list to
tuples. The tuple contains the original syntax subobject, and its computed
string representation. This example aims to showcase how paramorphisms work by
returning a final list of tuples that mimics the intermediate tuple shapes the
algebra receives throughout the bottom up traversal.

Example Usage:
let terms = indexedTerm ["leaf1", "leaf2", "leaf3"]
termPara terms = Recurse over the structure to start at the leaves (bottom up traversal):

tuple3 = ( CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf3")), "leaf3" ) : []

Continue the traversal from leaves to root:

tuple2:3 = ( CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf2")), "leaf2") : tuple3

tuple1:2:3 = ( CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf1" )), "leaf1") : tuple2:3

Compute the root:
tupleIndexed:1:2:3 = ( CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Indexed [])), "indexed" ) : tuple1:2:3

Final shape:
[ (CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Indexed []))  , "indexed")
, (CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf1")), "leaf1")
, (CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf2")), "leaf2")
, (CofreeT (Identity ((Range {start = 1, end = 10} :. MethodCall :. Nil) :< Leaf "leaf3")), "leaf3")
]

-}
termPara :: Term Syntax (Record '[Range, Category]) -> [(Term Syntax (Record '[Range, Category]), Text)]
termPara = para algebra
  where
    algebra term = case term of
      (annotation :< Leaf representation) -> [(cofree (annotation :< Leaf representation), representation)]
      (annotation :< Indexed values) -> [(cofree (annotation :< Indexed []), "indexed")] <> (values >>= Prelude.snd)
      _ -> [(cofree ((Range 1 10 :. Category.MethodCall :. Nil) :< Leaf "unknown"), "unknown")]
