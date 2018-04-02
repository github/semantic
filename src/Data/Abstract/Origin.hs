{-# LANGUAGE GADTs, RankNTypes, UndecidableInstances #-}
module Data.Abstract.Origin where

import qualified Data.Abstract.Module as M
import qualified Data.Abstract.Package as P
import Prologue

-- | An 'Origin' encapsulates the location at which a name is bound or allocated.
data Origin term ty where
  Unknown ::                                    Origin term any
  Package ::                   P.PackageInfo -> Origin term 'P
  Module  :: Origin term 'P -> M.ModuleInfo  -> Origin term 'M
  Term    :: Origin term 'M -> Base term ()  -> Origin term 'T

-- | A type index indicating the finest grain of information available in a given 'Origin'.
data OriginType = P | M | T
  deriving (Eq, Ord, Show)

-- | Project the 'ModuleInfo' out of an 'Origin', if available.
originModule :: Origin term ty -> Maybe M.ModuleInfo
originModule (Term o _)   = originModule o
originModule (Module _ m) = Just m
originModule _            = Nothing

-- | Project the 'PackageInfo' out of an 'Origin', if available.
originPackage :: Origin term ty -> Maybe P.PackageInfo
originPackage (Term o _)   = originPackage o
originPackage (Module o _) = originPackage o
originPackage (Package p)  = Just p
originPackage _            = Nothing

deriving instance Eq (Base term ()) => Eq (Origin term ty)
deriving instance Show (Base term ()) => Show (Origin term ty)

-- | Compare two origins with arbitrary type indices using a function to compare term functors.
liftCompareOrigins :: (Base term () -> Base term () -> Ordering) -> Origin term ty1 -> Origin term ty2 -> Ordering
liftCompareOrigins _ Unknown        Unknown        = EQ
liftCompareOrigins _ Unknown        _              = LT
liftCompareOrigins _ _              Unknown        = GT
liftCompareOrigins _ (Package p1)   (Package p2)   = compare p1 p2
liftCompareOrigins _ (Package _)    _              = LT
liftCompareOrigins _ _              (Package _)    = GT
liftCompareOrigins c (Module p1 m1) (Module p2 m2) = liftCompareOrigins c p1 p2 <> compare m1 m2
liftCompareOrigins _ (Module _ _)   _              = LT
liftCompareOrigins _ _              (Module _ _)   = GT
liftCompareOrigins c (Term m1 t1)   (Term m2 t2)   = liftCompareOrigins c m1 m2 <> c t1 t2

instance Ord (Base term ()) => Ord (Origin term ty) where
  compare = liftCompareOrigins compare

-- | An existential abstraction over 'Origin's of different types.
data SomeOrigin term where
  SomeOrigin :: Origin term ty -> SomeOrigin term

packageOrigin :: P.Package term -> SomeOrigin term
packageOrigin = SomeOrigin . Package . P.packageInfo

moduleOrigin :: M.Module term -> SomeOrigin term
moduleOrigin = SomeOrigin . Module Unknown . M.moduleInfo

termOrigin :: Recursive term => term -> SomeOrigin term
termOrigin = SomeOrigin . Term Unknown . (() <$) . project

withSomeOrigin :: (forall ty . Origin term ty -> b) -> SomeOrigin term -> b
withSomeOrigin with (SomeOrigin o) = with o

instance Eq (Base term ()) => Eq (SomeOrigin term) where
  SomeOrigin o1 == SomeOrigin o2 = liftCompareOrigins (\ t1 t2 -> if t1 == t2 then EQ else LT) o1 o2 == EQ

instance Ord (Base term ()) => Ord (SomeOrigin term) where
  compare (SomeOrigin o1) (SomeOrigin o2) = liftCompareOrigins compare o1 o2

deriving instance Show (Base term ()) => Show (SomeOrigin term)


merge :: Origin term ty1 -> Origin term ty2 -> SomeOrigin term
merge a                     Unknown                     = SomeOrigin a
merge (Package p)           (Module Unknown m)          = SomeOrigin (Module (Package p) m)
merge (Module p _)          (Module Unknown m)          = SomeOrigin (Module p m)
merge (Term (Module p _) _) (Module Unknown m)          = SomeOrigin (Module p m)
merge (Term (Module p _) _) (Term (Module Unknown m) t) = SomeOrigin (Term (Module p m) t)
merge (Module p m)          (Term Unknown t)            = SomeOrigin (Term (Module p m) t)
merge (Term m _)            (Term Unknown t)            = SomeOrigin (Term m t)
merge _                     b                           = SomeOrigin b

instance Semigroup (SomeOrigin term) where
  SomeOrigin a <> SomeOrigin b = merge a b

instance Monoid (SomeOrigin term) where
  mempty = SomeOrigin Unknown
  mappend = (<>)
