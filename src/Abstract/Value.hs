{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances, TypeApplications #-}
module Abstract.Value where

import Abstract.Environment
import Abstract.Store
import qualified Abstract.Type as Type
import Abstract.FreeVariables
import Control.Monad hiding (fail)
import Data.ByteString (ByteString)
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import Data.Union
import GHC.Generics
import Prelude hiding (fail)

type ValueConstructors syntax ann
  = '[Closure syntax ann
    , Abstract.Value.Unit
    , Abstract.Value.Boolean
    , Abstract.Value.Integer
    , Abstract.Value.String
    ]

type Value syntax ann = Union (ValueConstructors syntax ann)

data Closure syntax ann location = Closure [Name] (Term syntax ann) (Environment location (Value syntax ann location))
  deriving (Eq, Ord, Show)

instance (Eq1 syntax, Eq ann) => Eq1 (Closure syntax ann) where
  liftEq eqL (Closure s1 t1 e1) (Closure s2 t2 e2) = s1 == s2 && t1 == t2 && liftEq2 eqL (liftEq eqL) e1 e2

instance (Ord1 syntax, Ord ann) => Ord1 (Closure syntax ann) where
  liftCompare compareL (Closure s1 t1 e1) (Closure s2 t2 e2) = compare s1 s2 <> compare t1 t2 <> liftCompare2 compareL (liftCompare compareL) e1 e2

instance (Show1 syntax, Show ann) => Show1 (Closure syntax ann) where
  liftShowsPrec sp sl d (Closure s t e) = showParen (d > 10) $ showString "Closure"
    . showChar ' ' . showsPrec 11 s
    . showChar ' ' . showsPrec 11 t
    . showChar ' ' . liftShowsPrec2 sp sl (liftShowsPrec sp sl) (liftShowList sp sl) 11 e

data Unit location = Unit
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Unit where liftEq = genericLiftEq
instance Ord1 Unit where liftCompare = genericLiftCompare
instance Show1 Unit where liftShowsPrec = genericLiftShowsPrec

data Boolean location = Boolean Prelude.Bool
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

data Integer location = Integer Prelude.Integer
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Abstract.Value.Integer where liftEq = genericLiftEq
instance Ord1 Abstract.Value.Integer where liftCompare = genericLiftCompare
instance Show1 Abstract.Value.Integer where liftShowsPrec = genericLiftShowsPrec

data String location = String ByteString
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Abstract.Value.String where liftEq = genericLiftEq
instance Ord1 Abstract.Value.String where liftCompare = genericLiftCompare
instance Show1 Abstract.Value.String where liftShowsPrec = genericLiftShowsPrec


type family LocationFor value :: * where
  LocationFor (Value syntax ann location) = location
  LocationFor Type.Type = Monovariant


-- Instances

class ValueRoots l v | v -> l where
  valueRoots :: v -> Set.Set (Address l v)

class AbstractValue v where
  unit :: v
  integer :: Prelude.Integer -> v
  boolean :: Bool -> v
  string :: ByteString -> v

instance (FreeVariables1 syntax, Functor syntax, Ord l) => ValueRoots l (Value syntax ann l) where
  valueRoots v
    | Just (Closure names body env) <- prj v = envRoots env (foldr Set.delete (freeVariables body) names)
    | otherwise                              = mempty

instance AbstractValue (Value syntax ann l) where
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
