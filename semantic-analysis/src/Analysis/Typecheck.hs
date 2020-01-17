{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (..)
, typecheckingFlowInsensitive
) where

import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import qualified Analysis.Effect.Domain as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Functor.Named
import qualified Analysis.Intro as Intro
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict as Fresh
import           Control.Carrier.Reader hiding (Local)
import           Control.Carrier.State.Strict
import           Control.Monad (unless)
import           Control.Monad.Trans.Class
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Functor (($>))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Semigroup (Last (..))
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Data.Void
import           GHC.Generics (Generic1)
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Module
import           Syntax.Scope
import           Syntax.Term
import           Syntax.Var (closed)
import qualified System.Path as Path

data Monotype f a
  = Bool
  | Unit
  | String
  | f a :-> f a
  | Record (Map.Map Name (f a))
  deriving (Foldable, Functor, Generic1, Traversable)

infixr 0 :->

type Type = Term Monotype Meta

type Addr = Name

-- FIXME: Union the effects/annotations on the operands.

-- | We derive the 'Semigroup' instance for types to take the second argument. This is equivalent to stating that the type of an imperative sequence of statements is the type of its final statement.
deriving via (Last (Term Monotype a)) instance Semigroup (Term Monotype a)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Monotype f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Monotype f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Monotype f a)

instance HFunctor Monotype
instance RightModule Monotype where
  Unit      >>=* _ = Unit
  Bool      >>=* _ = Bool
  String    >>=* _ = String
  (a :-> b) >>=* f = a >>= f :-> b >>= f
  Record m  >>=* f = Record ((>>= f) <$> m)

type Meta = Int

newtype Polytype f a = PForAll (Scope () f a)
  deriving (Foldable, Functor, Generic1, HFunctor, RightModule, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Polytype f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Polytype f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Polytype f a)


forAll :: (Eq a, Has Polytype sig m) => a -> m a -> m a
forAll n body = send (PForAll (abstract1 n body))

forAlls :: (Eq a, Has Polytype sig m, Foldable t) => t a -> m a -> m a
forAlls ns body = foldr forAll body ns

generalize :: Term Monotype Meta -> Term (Polytype :+: Monotype) Void
generalize ty = fromJust (closed (forAlls (IntSet.toList (mvs ty)) (hoistTerm R ty)))


typecheckingFlowInsensitive
  :: (Has Intro.Intro syn term, Ord (term Addr))
  => (forall sig m
     .  (Has (A.Domain term Addr Type :+: Env Addr :+: A.Heap Addr Type :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m Type)
     -> (term Addr -> m Type)
     )
  -> [File (term Addr)]
  -> ( Heap Type
     , [File (Either (Path.AbsRelFile, Span, String) (Term (Polytype :+: Monotype) Void))]
     )
typecheckingFlowInsensitive eval
  = run
  . evalFresh 0
  . runHeap
  . fmap (fmap (fmap (fmap generalize)))
  . traverse (runFile eval)

runFile
  :: ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap Type)) sig m
     , Has Intro.Intro syn term
     , Ord (term Addr)
     )
  => (forall sig m
     .  (Has (A.Domain term Addr Type :+: Env Addr :+: A.Heap Addr Type :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m Type)
     -> (term Addr -> m Type)
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) Type))
runFile eval file = traverse run file
  where run
          = (\ m -> do
              (subst, t) <- m
              modify @(Heap Type) (fmap (Set.map (substAll subst)))
              pure (substAll subst <$> t))
          . runState @Substitution mempty
          . runReader (filePath file)
          . runReader (fileSpan file)
          . runEnv
          . runFail
          . (\ m -> do
            (cs, t) <- m
            t <$ solve cs)
          . runState @(Set.Set Constraint) mempty
          . (\ m -> do
              v <- meta
              bs <- m
              v <$ for_ bs (unify v))
          . convergeTerm 1  (A.runHeap @Addr @Type . fix (\ eval' -> runDomain eval' . fix (cacheTerm . eval)))


data Constraint = Type :===: Type
  deriving (Eq, Ord, Show)

infix 4 :===:

data Solution
  = Int := Type
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: Has Fresh sig m => m Type
meta = pure <$> Fresh.fresh

unify :: Has (State (Set.Set Constraint)) sig m => Type -> Type -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1 :===: t2))

type Substitution = IntMap.IntMap Type

solve :: (Has (State Substitution) sig m, MonadFail m) => Set.Set Constraint -> m ()
solve cs = for_ cs solve
  where solve = \case
          -- FIXME: how do we enforce proper subtyping? row polymorphism or something?
          Alg (Record f1) :===: Alg (Record f2) -> traverse solve (Map.intersectionWith (:===:) f1 f2) $> ()
          Alg (a1 :-> b1) :===: Alg (a2 :-> b2) -> solve (a1 :===: a2) *> solve (b1 :===: b2)
          Var m1   :===: Var m2   | m1 == m2 -> pure ()
          Var m1   :===: t2         -> do
            sol <- solution m1
            case sol of
              Just (_ := t1) -> solve (t1 :===: t2)
              Nothing | m1 `IntSet.member` mvs t2 -> fail ("Occurs check failure: " <> show m1 <> " :===: " <> show t2)
                      | otherwise                 -> modify (IntMap.insert m1 t2 . fmap (substAll (IntMap.singleton m1 t2)))
          t1         :===: Var m2   -> solve (Var m2 :===: t1)
          t1         :===: t2       -> unless (t1 == t2) $ fail ("Type mismatch:\nexpected: " <> show t1 <> "\n  actual: " <> show t2)

        solution m = fmap (m :=) <$> gets (IntMap.lookup m)


mvs :: Foldable t => t Meta -> IntSet.IntSet
mvs = foldMap IntSet.singleton

substAll :: Monad t => IntMap.IntMap (t Meta) -> t Meta -> t Meta
substAll s a = a >>= \ i -> fromMaybe (pure i) (IntMap.lookup i s)


runDomain :: (term Addr -> m Type) -> DomainC term m a -> m a
runDomain eval (DomainC m) = runReader eval m

newtype DomainC term m a = DomainC (ReaderC (term Addr -> m Type) m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

instance MonadTrans (DomainC term) where
  lift = DomainC . lift

instance ( Alternative m
         , Has (Env Addr) sig m
         , Has Fresh sig m
         , Has (A.Heap Addr Type) sig m
         , Has (State (Set.Set Constraint)) sig m
         , Monad term
         , MonadFail m
         , Has Intro.Intro syn term
         )
      => Algebra (A.Domain term Addr Type :+: sig) (DomainC term m) where
  alg = \case
    L (L (A.Unit       k)) -> k (Alg Unit)
    L (R (L (A.Bool     _ k))) -> k (Alg Bool)
    L (R (L (A.AsBool   t k))) -> do
      unify t (Alg Bool)
      k True <|> k False
    L (R (R (L (A.String   _ k)))) -> k (Alg String)
    L (R (R (L (A.AsString t k)))) -> do
      unify t (Alg String)
      k mempty
    L (R (R (R (L (A.Lam (Named n b) k))))) -> do
      eval <- DomainC ask
      addr <- alloc @Name n
      arg <- meta
      A.assign addr arg
      ty <- lift (eval (instantiate1 (pure n) b))
      k (Alg (arg :-> ty))
    L (R (R (R (L (A.AsLam    t k))))) -> do
      arg <- meta
      ret <- meta
      unify t (Alg (arg :-> ret))
      b <- concretize ret
      k (Named (name mempty) (lift b)) where
      concretize = \case
        Alg Unit       -> pure Intro.unit
        Alg Bool       -> pure (Intro.bool True) <|> pure (Intro.bool False)
        Alg String     -> pure (Intro.string mempty)
        Alg (_ :-> b)  -> send . Intro.Lam . Named (name mempty) . lift <$> concretize b
        Alg (Record t) -> Intro.record <$> traverse (traverse concretize) (Map.toList t)
        t              -> fail $ "can’t concretize " <> show t -- FIXME: concretize type variables by incrementally solving constraints
    L (R (R (R (R (A.Record fields k))))) -> do
      eval <- DomainC ask
      fields' <- for fields $ \ (k, t) -> do
        addr <- alloc @Addr k
        v <- lift (eval t)
        (k, v) <$ A.assign addr v
      -- FIXME: should records reference types by address instead?
      k (Alg (Record (Map.fromList fields')))
    L (R (R (R (R (A.AsRecord t k))))) -> do
      unify t (Alg (Record mempty))
      k mempty -- FIXME: return whatever fields we have, when it’s actually a Record

    R other -> DomainC (send (handleCoercible other))
