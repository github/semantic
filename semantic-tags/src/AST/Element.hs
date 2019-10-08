{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}
module AST.Element
( Element(..)
, pattern Prj
) where

import GHC.Generics ((:+:)(..))
import GHC.TypeLits (ErrorMessage(..), TypeError)

-- | Projection of an element out of a tree of sums, particularly suitable for use with highly branching (e.g. balanced) trees.
class Element sub sup where
  -- | Project one element out of a sum type.
  prj :: sup a -> Maybe (sub a)

instance (Element' elem sub sup, elem ~ Elem sub sup) => Element sub sup where
  prj = prj' @elem


pattern Prj :: Element sub sup => sub a -> sup a
pattern Prj sub <- (prj -> Just sub)

data Side = None | Here | L | R

type family Elem sub sup :: Side where
  Elem t t         = 'Here
  Elem t (l :+: r) = Elem' 'L t l <> Elem' 'R t r
  Elem _ _         = 'None

type family Elem' (side :: Side) sub sup :: Side where
  Elem' s t t         = s
  Elem' s t (l :+: r) = Elem' s t l <> Elem' s t r
  Elem' _ _ _         = 'None

type family (a :: Side) <> (b :: Side) :: Side where
  'None <> b = b
  a     <> _ = a

class Element' (elem :: Side) sub sup where
  prj' :: sup a -> Maybe (sub a)

instance {-# OVERLAPPABLE #-}
         Element' 'Here t t where
  prj' = Just

instance {-# OVERLAPPABLE #-}
         Element     t l
      => Element' 'L t (l :+: r) where
  prj' (L1 l) = prj l
  prj' _      = Nothing

instance {-# OVERLAPPABLE #-}
         Element     t r
      => Element' 'R t (l :+: r) where
  prj' (R1 r) = prj r
  prj' _      = Nothing


type family ShowSum t where
  ShowSum (l :+: r) = ShowSum' ('Text "{ ") (l :+: r) ':$$: 'Text "}"
  ShowSum t         = 'Text "{ " ':<>: 'ShowType t ':<>: 'Text " }"

type family ShowSum' p t where
  ShowSum' p (l :+: r) = ShowSum' p l ':$$: ShowSum' ('Text ", ") r
  ShowSum' p t         = p ':<>: 'ShowType t

instance TypeError
           (     'ShowType t ':<>: 'Text " is not in"
           ':$$: ShowSum u)
      => Element' 'None t u where
  prj' _ = Nothing
