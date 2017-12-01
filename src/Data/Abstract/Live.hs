{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Live where

import Data.Abstract.Address
import Data.Functor.Classes.Generic
import Data.Semigroup
import Data.Set as Set
import GHC.Generics
import Unsafe.Coerce

newtype Live l v = Live { unLive :: Set (Address l v) }
  deriving (Eq, Foldable, Monoid, Ord, Semigroup, Show)

singleton :: Address l v -> Live l v
singleton = Live . Set.singleton

delete :: Ord l => Address l v -> Live l v -> Live l v
delete addr (Live s) = Live (Set.delete addr s)


instance Generic1 (Live l) where
  type Rep1 (Live l)
    = D1
        ('MetaData "Live" "Data.Abstract.Live" "main" 'Prelude.True)
        (C1
           ('MetaCons "Live" 'PrefixI 'Prelude.True)
           (S1
              ('MetaSel
                 ('Just "unLive")
                 'NoSourceUnpackedness
                 'NoSourceStrictness
                 'DecidedLazy)
              (Set :.: Rec1 (Address l))))
  to1 = unsafeCoerce
  from1 = unsafeCoerce

instance Ord l => Functor (Live l) where
  fmap _ = unsafeCoerce

instance Eq l => Eq1 (Live l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Live l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Live l) where liftShowsPrec = genericLiftShowsPrec
