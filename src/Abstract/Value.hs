{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances, TypeApplications #-}
module Abstract.Value where

import Abstract.Environment
import Abstract.Store
import qualified Abstract.Type as Type
import Abstract.FreeVariables
import Data.ByteString (ByteString)
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Semigroup
import qualified Data.Set as Set
import Data.Union
import GHC.Generics
import Prelude hiding (fail)

type ValueConstructors location
  = '[Closure location
    , Abstract.Value.Unit
    , Abstract.Value.Boolean
    , Abstract.Value.Integer
    , Abstract.Value.String
    ]

type Value location = Union (ValueConstructors location)

data Closure location term = Closure [Name] term (Environment location (Value location term))
  deriving (Eq, Ord, Show)

instance (Eq location) => Eq1 (Closure location) where
  liftEq eqT (Closure s1 t1 e1) (Closure s2 t2 e2) = s1 == s2 && t1 `eqT` t2 && liftEq (liftEq eqT) e1 e2

instance (Ord location) => Ord1 (Closure location) where
  liftCompare compareT (Closure s1 t1 e1) (Closure s2 t2 e2) = compare s1 s2 <> compareT t1 t2 <> liftCompare (liftCompare compareT) e1 e2

instance (Show location) => Show1 (Closure location) where
  liftShowsPrec spT slT d (Closure s t e) = showParen (d > 10) $ showString "Closure"
    . showChar ' ' . showsPrec 11 s
    . showChar ' ' . spT 11 t
    . showChar ' ' . liftShowsPrec (liftShowsPrec spT slT) (liftShowList spT slT) 11 e

data Unit term = Unit
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Unit where liftEq = genericLiftEq
instance Ord1 Unit where liftCompare = genericLiftCompare
instance Show1 Unit where liftShowsPrec = genericLiftShowsPrec

data Boolean term = Boolean Prelude.Bool
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

data Integer term = Integer Prelude.Integer
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Abstract.Value.Integer where liftEq = genericLiftEq
instance Ord1 Abstract.Value.Integer where liftCompare = genericLiftCompare
instance Show1 Abstract.Value.Integer where liftShowsPrec = genericLiftShowsPrec

data String term = String ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Abstract.Value.String where liftEq = genericLiftEq
instance Ord1 Abstract.Value.String where liftCompare = genericLiftCompare
instance Show1 Abstract.Value.String where liftShowsPrec = genericLiftShowsPrec


type family LocationFor value :: * where
  LocationFor (Value location term) = location
  LocationFor Type.Type = Monovariant


-- Instances

class ValueRoots l v | v -> l where
  valueRoots :: v -> Set.Set (Address l v)

class AbstractValue v where
  unit :: v
  integer :: Prelude.Integer -> v
  boolean :: Bool -> v
  string :: ByteString -> v

instance (FreeVariables term, Ord location) => ValueRoots location (Value location term) where
  valueRoots v
    | Just (Closure names body env) <- prj v = envRoots env (foldr Set.delete (freeVariables body) names)
    | otherwise                              = mempty

instance AbstractValue (Value location term) where
  unit = inj Unit
  integer = inj . Abstract.Value.Integer
  boolean = inj . Boolean
  string = inj . Abstract.Value.String

instance ValueRoots Monovariant Type.Type where
  valueRoots _ = mempty

instance AbstractValue Type.Type where
  unit = Type.Unit
  integer _ = Type.Int
  boolean _ = Type.Bool
  string _ = Type.String
