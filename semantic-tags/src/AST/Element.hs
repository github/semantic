{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module AST.Element
( Element(..)
) where

import GHC.Generics
import GHC.TypeLits (ErrorMessage(..), TypeError)

class Element sub sup where
  prj :: sup a -> Maybe (sub a)

instance (Element' elem sub sup, elem ~ Elem sub sup) => Element sub sup where
  prj = prj' @elem


type family Elem sub sup where
  Elem t t         = 'True
  Elem t (l :+: r) = Elem t l || Elem t r
  Elem _ _         = 'False

type family a || b where
  'True || _     = 'True
  _     || 'True = 'True
  _     || _     = 'False

class Element' (elem :: Bool) sub sup where
  prj' :: sup a -> Maybe (sub a)

instance {-# OVERLAPPABLE #-}
         Element' 'True t t where
  prj' = Just

instance {-# OVERLAPPABLE #-}
         Element' 'True t (l1 :+: l2 :+: r)
      => Element' 'True t ((l1 :+: l2) :+: r) where
  prj' = prj' @'True . reassoc where
    reassoc (L1 (L1 l)) = L1 l
    reassoc (L1 (R1 l)) = R1 (L1 l)
    reassoc (R1 r)      = R1 (R1 r)

instance {-# OVERLAPPABLE #-}
         Element' 'True t (t :+: r) where
  prj' (L1 l) = Just l
  prj' _      = Nothing

instance {-# OVERLAPPABLE #-}
         Element' 'True t r
      => Element' 'True t (l :+: r) where
  prj' (R1 r) = prj' @'True r
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
      => Element' 'False t u where
  prj' _ = Nothing
