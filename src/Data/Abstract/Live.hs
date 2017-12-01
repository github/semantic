{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Live where

import Data.Abstract.Address
import Data.Bifunctor (second)
import Data.Function (on)
import Data.Functor.Classes.Generic
import Data.Semigroup
import Data.Set as Set
import GHC.Generics
import Unsafe.Coerce

newtype Live l v = Live { unLive :: Set (Address l v) }
  deriving (Eq, Foldable, Monoid, Ord, Semigroup, Show)

liveSingleton :: Address l v -> Live l v
liveSingleton = Live . Set.singleton

liveInsert :: Ord l => Address l v -> Live l v -> Live l v
liveInsert addr = Live . Set.insert addr . unLive

liveDelete :: Ord l => Address l v -> Live l v -> Live l v
liveDelete addr = Live . Set.delete addr . unLive

liveDifference :: Ord l => Live l v -> Live l v -> Live l v
liveDifference = fmap Live . (Set.difference `on` unLive)

liveMember :: Ord l => Address l v -> Live l v -> Bool
liveMember addr = Set.member addr . unLive

liveSplit :: Ord l => Live l v -> Maybe (Address l v, Live l v)
liveSplit = fmap (second Live) . Set.minView . unLive


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
