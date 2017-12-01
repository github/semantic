{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Live where

import Data.Abstract.Address
import Data.Set as Set
import GHC.Generics

newtype Live l v = Live { unLive :: Set (Address l v) }

instance Ord l => Generic1 (Live l) where
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
  to1 = Live . Set.map unRec1 . unComp1 . unM1 . unM1 . unM1
  from1 = M1 . M1 . M1 . Comp1 . Set.map Rec1 . unLive


instance Ord l => Functor (Live l) where
  fmap f (Live as) = Live (Set.map (fmap f) as)
