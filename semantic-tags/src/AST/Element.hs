{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}
module AST.Element
( Element(..)
, pattern Prj
, (:+:)(..)
) where

import GHC.Generics ((:+:)(..))
import GHC.TypeLits (ErrorMessage(..), TypeError)

-- | Projection of an element out of a tree of sums, particularly suitable for use with highly branching (e.g. balanced) trees. This provides custom type errors when the tree is known statically, rather than provided via a constraint passed on to callers.
class Element sub sup where
  -- | Project one element out of a sum type.
  prj :: sup a -> Maybe (sub a)

instance (Element' side sub sup, side ~ Find sub sup) => Element sub sup where
  prj = prj' @side


-- | A pattern synonym to conveniently project out matching elements.
pattern Prj :: Element sub sup => sub a -> sup a
pattern Prj sub <- (prj -> Just sub)


-- | Where does the element occur in the tree?
data Side = None | Here | L | R

-- | Find where in a tree an element occurs.
type family Find sub sup :: Side where
  Find t t         = 'Here
  Find t (l :+: r) = Find' 'L t l <> Find' 'R t r
  Find _ _         = 'None

-- | Helper to compute the side an element occurs on.
type family Find' (side :: Side) sub sup :: Side where
  Find' s t t         = s
  Find' s t (l :+: r) = Find' s t l <> Find' s t r
  Find' _ _ _         = 'None

-- | Return the leftmost non-'None' side, or 'None'.
type family (a :: Side) <> (b :: Side) :: Side where
  'None <> b = b
  a     <> _ = a

-- | Helper to project elements out of the matching portion of a tree.
class Element' (side :: Side) sub sup where
  prj' :: sup a -> Maybe (sub a)

-- | Membership is reflexive.
instance Element' 'Here t t where
  prj' = Just

-- | Membershp on the left.
instance Element     t l
      => Element' 'L t (l :+: r) where
  prj' (L1 l) = prj l
  prj' _      = Nothing

-- | Membership on the right.
instance Element     t r
      => Element' 'R t (l :+: r) where
  prj' (R1 r) = prj r
  prj' _      = Nothing


-- | Error cases, with custom type errors.
instance TypeError
           (     'ShowType t ':<>: 'Text " is not in"
           ':$$: ShowSum u)
      => Element' 'None t u where
  prj' _ = Nothing


-- | Show a sum tree as a set (for use in type errors).
type family ShowSum t where
  ShowSum (l :+: r) = ShowSum' ('Text "{ ") (l :+: r) ':$$: 'Text "}"
  ShowSum t         = 'Text "{ " ':<>: 'ShowType t ':<>: 'Text " }"

-- | Helper for formatting & aligning sums nicely with a prefix.
type family ShowSum' p t where
  ShowSum' p (l :+: r) = ShowSum' p l ':$$: ShowSum' ('Text ", ") r
  ShowSum' p t         = p ':<>: 'ShowType t
