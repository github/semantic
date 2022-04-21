{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (..)
, typecheckingFlowInsensitive
) where

import           Analysis.Carrier.Fail.WithLoc
import qualified Analysis.Carrier.Store.Monovariant as A
import           Analysis.Effect.Domain as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Functor.Named
import           Analysis.Reference
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fresh.Strict as Fresh
import           Control.Carrier.Reader hiding (Local)
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Monad (ap, guard, unless)
import           Control.Monad.Trans.Class
import           Data.Foldable (for_, sequenceA_)
import           Data.Function (fix)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Semigroup (Last (..))
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic1)
import           Prelude hiding (fail)

data Monotype a
  = Var a
  | Unit
  | Bool
  | Int
  | String
  | [Monotype a] :-> Monotype a
  -- | (Locally) undefined names whose types are unknown. May not be eliminated by unification.
  | TypeOf Name
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

infixr 0 :->

-- FIXME: Union the effects/annotations on the operands.

-- | We derive the 'Semigroup' instance for types to take the second argument. This is equivalent to stating that the type of an imperative sequence of statements is the type of its final statement.
deriving via (Last (Monotype a)) instance Semigroup (Monotype a)

instance Applicative Monotype where
  pure = Var
  (<*>) = ap

instance Monad Monotype where
  m >>= f = case m of
    Var a    -> f a
    Unit     -> Unit
    Bool     -> Bool
    Int      -> Int
    String   -> String
    TypeOf n -> TypeOf n
    a :-> b  -> (map (>>= f) a) :-> (b >>= f)


type Type = Monotype Meta


type Meta = Int


data Polytype a
  = PForAll (Polytype (Maybe a))
  | PType (Monotype a)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)

abstract :: Eq a => a -> Polytype a -> Polytype (Maybe a)
abstract n = fmap (\ a -> a <$ guard (a == n))


forAll :: Eq a => a -> Polytype a -> Polytype a
forAll n body = PForAll (abstract n body)

forAlls :: (Eq a, Foldable t) => t a -> Polytype a -> Polytype a
forAlls ns body = foldr forAll body ns

generalize :: Monotype Meta -> Polytype Void
generalize ty = fromJust (closed (forAlls (IS.toList (mvs ty)) (PType ty)))


typecheckingFlowInsensitive
  :: Ord term
  => (forall sig m
     .  (Has (A.Dom Type :+: A.Env A.MAddr :+: Reader Reference) sig m, HasLabelled A.Store (A.Store A.MAddr Type) sig m, MonadFail m)
     => (term -> m Type)
     -> (term -> m Type)
     )
  -> [File term]
  -> ( A.MStore Type
     , [File (Either (Reference, String) (Polytype Void))]
     )
typecheckingFlowInsensitive eval
  = run
  . evalFresh 0
  . A.runStoreState
  . fmap (fmap (fmap (fmap generalize)))
  . traverse (runFile eval)

runFile
  :: ( Has Fresh sig m
     , Has (State (A.MStore Type)) sig m
     , Ord term
     )
  => (forall sig m
     .  (Has (A.Dom Type :+: A.Env A.MAddr :+: Reader Reference) sig m, HasLabelled A.Store (A.Store A.MAddr Type) sig m, MonadFail m)
     => (term -> m Type)
     -> (term -> m Type)
     )
  -> File term
  -> m (File (Either (Reference, String) Type))
runFile eval file = traverse run file
  where run
          = (\ m -> do
              (subst, t) <- m
              modify @(A.MStore Type) (A.MStore . fmap (Set.map (substAll subst)) . A.getMStore)
              pure (substAll subst <$> t))
          . runState @Substitution mempty
          . runReader (fileRef file)
          . A.runEnv @Type
          . runFail
          . (\ m -> do
            (cs, t) <- m
            t <$ solve cs)
          . runState @(Set.Set (Type, Type)) mempty
          . (\ m -> do
              v <- meta
              bs <- m
              v <$ for_ bs (unify v))
          . convergeTerm (A.runStore @Type . runDomain . fix (cacheTerm . eval))


data Solution
  = Int := Type
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: Has Fresh sig m => m Type
meta = pure <$> Fresh.fresh

unify :: Has (State (Set.Set (Type, Type))) sig m => Type -> Type -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1, t2))

type Substitution = IM.IntMap Type

solve :: (Has (State Substitution) sig m, MonadFail m) => Set.Set (Type, Type) -> m ()
solve cs = for_ cs (uncurry solve)
  where solve = curry $ \case
          (a1 :-> b1, a2 :-> b2)            -> sequenceA_ (zipWith solve a1 a2) *> solve b1 b2
          (Var m1   , Var m2)    | m1 == m2 -> pure ()
          (Var m1   , t2)                   -> do
            sol <- solution m1
            case sol of
              Just (_ := t1)                  -> solve t1 t2
              Nothing | m1 `IS.member` mvs t2 -> fail ("Occurs check failure: " <> show m1 <> " :===: " <> show t2)
                      | otherwise             -> modify (IM.insert m1 t2 . fmap (substAll (IM.singleton m1 t2)))
          (t1       , Var m2)               -> solve (Var m2) t1
          (t1       , t2)                   -> unless (t1 == t2) $ fail ("Type mismatch:\nexpected: " <> show t1 <> "\n  actual: " <> show t2)

        solution m = fmap (m :=) <$> gets (IM.lookup m)


mvs :: Foldable t => t Meta -> IS.IntSet
mvs = foldMap IS.singleton

substAll :: Monad t => IM.IntMap (t Meta) -> t Meta -> t Meta
substAll s a = a >>= \ i -> fromMaybe (pure i) (IM.lookup i s)


newtype DomainC m a = DomainC { runDomain :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

instance MonadTrans DomainC where
  lift = DomainC

instance ( Alternative m
         , Has (A.Env A.MAddr) sig m
         , Has Fresh sig m
         , HasLabelled A.Store (A.Store A.MAddr Type) sig m
         , Has (State (Set.Set (Type, Type))) sig m
         , MonadFail m
         )
      => Algebra (A.Dom Type :+: sig) (DomainC m) where
  alg hdl sig ctx = case sig of
    L (DVar n)  -> pure (TypeOf n <$ ctx)

    L (DInt _)  -> pure (Int <$ ctx)

    L DUnit     -> pure (Unit <$ ctx)

    L (DBool _) -> pure (Bool <$ ctx)
    L (DIf c t e) -> do
      unify c Bool
      hdl (t <$ ctx) <|> hdl (e <$ ctx)

    L (DString _) -> pure (String <$ ctx)

    L (DAbs n b) -> do
      addrs <- traverse A.alloc n
      args <- traverse (const meta) n
      sequenceA_ (zipWith (A..=) addrs args)
      ty <- hdl (b args <$ ctx)
      pure ((args :->) <$> ty)
    L (DApp f a) -> do
      args <- traverse (const meta) a
      ret <- meta
      unify f (args :-> ret)
      sequenceA_ (zipWith unify a args)
      pure (ret <$ ctx)

    L (DDie msg) -> fail (show msg)

    R other -> DomainC (alg (runDomain . hdl) other ctx)
