{-# LANGUAGE DeriveFoldable, DeriveGeneric, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Environment

import Data.List (intersperse)
import Data.Functor.Classes
import Data.Monoid
import GHC.Generics

data Configuration l t v
  = Configuration
    { configurationTerm :: t
    , configurationRoots :: [Address l v]
    , configurationEnvironment :: Environment l v
    , configurationStore :: Store l v
    }
    deriving (Generic1)

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Configuration l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Configuration l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Configuration l t v)
deriving instance (Ord l, Foldable (Cell l)) => Foldable (Configuration l t)


instance (Eq l, Eq1 (Cell l)) => Eq2 (Configuration l) where
  liftEq2 eqT eqV (Configuration t1 r1 e1 s1) (Configuration t2 r2 e2 s2) = eqT t1 t2 && liftEq (liftEq eqV) r1 r2 && liftEq eqV e1 e2 && liftEq eqV s1 s2

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Configuration l t) where
  liftEq = liftEq2 (==)

instance (Ord l, Ord1 (Cell l)) => Ord2 (Configuration l) where
  liftCompare2 compareT compareV (Configuration t1 r1 e1 s1) (Configuration t2 r2 e2 s2) = compareT t1 t2 <> liftCompare (liftCompare compareV) r1 r2 <> liftCompare compareV e1 e2 <> liftCompare compareV s1 s2

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Configuration l t) where
  liftCompare = liftCompare2 compare

showsConstructor :: String -> Int -> [Int -> ShowS] -> ShowS
showsConstructor name d fields = showParen (d > 10) $ showString name . showChar ' ' . foldr (.) id (intersperse (showChar ' ') ([($ 11)] <*> fields))


instance (Show l, Show1 (Cell l)) => Show2 (Configuration l) where
  liftShowsPrec2 spT _ spV slV d (Configuration t r e s) = showsConstructor "Configuration" d [ flip spT t, flip (liftShowsPrec (liftShowsPrec spV slV) (liftShowList spV slV)) r, flip (liftShowsPrec spV slV) e, flip (liftShowsPrec spV slV) s ]

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Configuration l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
