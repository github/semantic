{-# LANGUAGE DeriveGeneric, DeriveTraversable, DerivingVia, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, RecordWildCards, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (..)
, typecheckingFlowInsensitive
, typecheckingAnalysis
) where

import           Analysis.Analysis
import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Name
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Effect.Carrier
import           Control.Effect.Fresh as Fresh
import           Control.Effect.Reader hiding (Local)
import           Control.Effect.State
import           Control.Monad (unless)
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
  deriving (Foldable, Functor, Generic1, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Polytype f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Polytype f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Polytype f a)

instance HFunctor Polytype
instance RightModule Polytype where
  PForAll b >>=* f = PForAll (b >>=* f)


forAll :: (Eq a, Carrier sig m, Member Polytype sig) => a -> m a -> m a
forAll n body = send (PForAll (abstract1 n body))

forAlls :: (Eq a, Carrier sig m, Member Polytype sig, Foldable t) => t a -> m a -> m a
forAlls ns body = foldr forAll body ns

generalize :: Term Monotype Meta -> Term (Polytype :+: Monotype) Void
generalize ty = fromJust (closed (forAlls (IntSet.toList (mvs ty)) (hoistTerm R ty)))


typecheckingFlowInsensitive
  :: Ord (term Name)
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term Name Type m
     -> (term Name -> m Type)
     -> (term Name -> m Type)
     )
  -> [File (term Name)]
  -> ( Heap Type
     , [File (Either (Path.AbsRelFile, Span, String) (Term (Polytype :+: Monotype) Void))]
     )
typecheckingFlowInsensitive eval
  = run
  . runFresh
  . runHeap
  . fmap (fmap (fmap (fmap generalize)))
  . traverse (runFile eval)

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap Type)) sig
     , Ord (term Name)
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term Name Type m
     -> (term Name -> m Type)
     -> (term Name -> m Type)
     )
  -> File (term Name)
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
          . runEnv @Name
          . runFail
          . (\ m -> do
            (cs, t) <- m
            t <$ solve cs)
          . runState @(Set.Set Constraint) mempty
          . (\ m -> do
              v <- meta
              bs <- m
              v <$ for_ bs (unify v))
          . convergeTerm (A.runHeap @Name @Type . fix (cacheTerm . eval typecheckingAnalysis))

typecheckingAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member (Env Name Name) sig
     , Member Fresh sig
     , Member (A.Heap Name Type) sig
     , Member (State (Set.Set Constraint)) sig
     )
  => Analysis term Name Type m
typecheckingAnalysis = Analysis{..}
  where abstract eval name body = do
          -- FIXME: construct the associated scope
          addr <- alloc @Name @Name name
          arg <- meta
          A.assign addr arg
          ty <- eval body
          pure (Alg (arg :-> ty))
        apply _ f a = do
          _A <- meta
          _B <- meta
          unify (Alg (_A :-> _B)) f
          unify _A a
          pure _B
        unit = pure (Alg Unit)
        bool _ = pure (Alg Bool)
        asBool b = unify (Alg Bool) b >> pure True <|> pure False
        string _ = pure (Alg String)
        asString s = unify (Alg String) s $> mempty
        record fields = do
          fields' <- for fields $ \ (k, v) -> do
            addr <- alloc @Name @Name k
            (k, v) <$ A.assign addr v
          -- FIXME: should records reference types by address instead?
          pure (Alg (Record (Map.fromList fields')))
        _ ... m = pure (Just m)


data Constraint = Type :===: Type
  deriving (Eq, Ord, Show)

infix 4 :===:

data Solution
  = Int := Type
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: (Carrier sig m, Member Fresh sig) => m Type
meta = pure <$> Fresh.fresh

unify :: (Carrier sig m, Member (State (Set.Set Constraint)) sig) => Type -> Type -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1 :===: t2))

type Substitution = IntMap.IntMap Type

solve :: (Member (State Substitution) sig, MonadFail m, Carrier sig m) => Set.Set Constraint -> m ()
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
          t1         :===: t2         -> unless (t1 == t2) $ fail ("Type mismatch:\nexpected: " <> show t1 <> "\n  actual: " <> show t2)

        solution m = fmap (m :=) <$> gets (IntMap.lookup m)


mvs :: Foldable t => t Meta -> IntSet.IntSet
mvs = foldMap IntSet.singleton

substAll :: Monad t => IntMap.IntMap (t Meta) -> t Meta -> t Meta
substAll s a = a >>= \ i -> fromMaybe (pure i) (IntMap.lookup i s)
