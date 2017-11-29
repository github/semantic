{-# LANGUAGE ConstraintKinds, DataKinds, FunctionalDependencies, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances, TypeApplications #-}
module Abstract.Value where

import Abstract.Environment
import Abstract.Primitive
import Abstract.Store
import Abstract.Type
import Abstract.Eval
import Abstract.FreeVariables
import Control.Monad hiding (fail)
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Semigroup
import Data.Set
import Data.Term
import Data.Union
import GHC.Generics
import Prelude hiding (fail)

type Value syntax ann = Union '[Closure syntax ann, Primitive]

data Closure syntax ann location = Closure Name (Term syntax ann) (Environment location (Value syntax ann location))
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

data Primitive location = I Prim
  deriving (Eq, Generic1, Ord, Show)

instance Eq1 Primitive where liftEq = genericLiftEq
instance Ord1 Primitive where liftCompare = genericLiftCompare
instance Show1 Primitive where liftShowsPrec = genericLiftShowsPrec


type family LocationFor value :: * where
  LocationFor (Value syntax ann location) = location
  LocationFor Type = Monovariant


literal :: Prim -> Value syntax ann location
literal = inj . I


-- Instances

class AbstractValue l v | v -> l where
  valueRoots :: v -> Set (Address l v)

instance (FreeVariables1 syntax, Functor syntax, Ord l) => AbstractValue l (Value syntax ann l) where
  valueRoots v
    | Just (I _) <- prj v                   = mempty
    | Just (Closure name body env) <- prj v = envRoots env (delete name (freeVariables body))
    | otherwise                             = error "valueRoots: unhandled constructor"

instance AbstractValue Monovariant Type where
  valueRoots _ = mempty


-- Eval instance

instance ( Monad m
         , Ord l
         , Functor s
         , MonadGC l (Value s a l) m
         , MonadEnv l (Value s a l) m
         , FreeVariables1 s)
         => Eval (Value s a l) m [] where
  eval _  yield []     = yield (literal PUnit)
  eval ev yield [a]    = ev pure a >>= yield
  eval ev yield (a:as) = do
    env <- askEnv @l @(Value s a l)
    extraRoots (envRoots @l env (freeVariables1 as)) (ev (const (eval ev pure as)) a) >>= yield
