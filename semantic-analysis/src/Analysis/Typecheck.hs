{-# LANGUAGE DeriveGeneric, DeriveTraversable, DerivingVia, FlexibleContexts, FlexibleInstances, LambdaCase, QuantifiedConstraints, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators #-}
module Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (..)
, typecheckingFlowInsensitive
, typecheckingAnalysis
) where

import           Analysis.Analysis
import           Analysis.File
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Effect.Carrier
import           Control.Effect.Fresh as Fresh
import           Control.Effect.Reader hiding (Local)
import           Control.Effect.State
import           Control.Monad ((>=>), unless)
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Functor (($>))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Proxy
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

data Monotype name f a
  = Bool
  | Unit
  | String
  | Arr (f a) (f a)
  | Record (Map.Map name (f a))
  deriving (Foldable, Functor, Generic1, Traversable)

type Type name = Term (Monotype name) Meta

-- FIXME: Union the effects/annotations on the operands.

-- | We derive the 'Semigroup' instance for types to take the second argument. This is equivalent to stating that the type of an imperative sequence of statements is the type of its final statement.
deriving via (Last (Term (Monotype name) a)) instance Semigroup (Term (Monotype name) a)

deriving instance (Eq   name, Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Monotype name f a)
deriving instance (Ord  name, Ord  a, forall a . Eq   a => Eq   (f a)
                                    , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Monotype name f a)
deriving instance (Show name, Show a, forall a . Show a => Show (f a))          => Show (Monotype name f a)

instance HFunctor (Monotype name)
instance RightModule (Monotype name) where
  Unit     >>=* _ = Unit
  Bool     >>=* _ = Bool
  String   >>=* _ = String
  Arr a b  >>=* f = Arr (a >>= f) (b >>= f)
  Record m >>=* f = Record ((>>= f) <$> m)

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

generalize :: Term (Monotype name) Meta -> Term (Polytype :+: Monotype name) Void
generalize ty = fromJust (closed (forAlls (IntSet.toList (mvs ty)) (hoistTerm R ty)))


typecheckingFlowInsensitive
  :: (Ord name, Ord (term name), Show name)
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term name name (Type name) m
     -> (term name -> m (Type name))
     -> (term name -> m (Type name))
     )
  -> [File (term name)]
  -> ( Heap name (Type name)
     , [File (Either (Path.AbsRelFile, Span, String) (Term (Polytype :+: Monotype name) Void))]
     )
typecheckingFlowInsensitive eval
  = run
  . runFresh
  . runHeap
  . fmap (fmap (fmap (fmap generalize)))
  . traverse (runFile eval)

runFile
  :: forall term name m sig
  .  ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap name (Type name))) sig
     , Ord name
     , Ord (term name)
     , Show name
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term name name (Type name) m
     -> (term name -> m (Type name))
     -> (term name -> m (Type name))
     )
  -> File (term name)
  -> m (File (Either (Path.AbsRelFile, Span, String) (Type name)))
runFile eval file = traverse run file
  where run
          = (\ m -> do
              (subst, t) <- m
              modify @(Heap name (Type name)) (fmap (Set.map (substAll subst)))
              pure (substAll subst <$> t))
          . runState (mempty :: (Substitution name))
          . runReader (filePath file)
          . runReader (fileSpan file)
          . runFail
          . (\ m -> do
            (cs, t) <- m
            t <$ solve @name cs)
          . runState (Set.empty :: Set.Set (Constraint name))
          . (\ m -> do
              v <- meta
              bs <- m
              v <$ for_ bs (unify v))
          . convergeTerm (Proxy @name) (fix (cacheTerm . eval typecheckingAnalysis))

typecheckingAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member Fresh sig
     , Member (State (Set.Set (Constraint name))) sig
     , Member (State (Heap name (Type name))) sig
     , Ord name
     )
  => Analysis term name name (Type name) m
typecheckingAnalysis = Analysis{..}
  where alloc = pure
        bind _ _ m = m
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr >=> nonEmpty . Set.toList) >>= maybe (pure Nothing) (foldMapA (pure . Just))
        assign addr ty = modify (Map.insertWith (<>) addr (Set.singleton ty))
        abstract eval name body = do
          -- FIXME: construct the associated scope
          addr <- alloc name
          arg <- meta
          assign addr arg
          ty <- eval body
          pure (Alg (Arr arg ty))
        apply _ f a = do
          _A <- meta
          _B <- meta
          unify (Alg (Arr _A _B)) f
          unify _A a
          pure _B
        unit = pure (Alg Unit)
        bool _ = pure (Alg Bool)
        asBool b = unify (Alg Bool) b >> pure True <|> pure False
        string _ = pure (Alg String)
        asString s = unify (Alg String) s $> mempty
        record fields = do
          fields' <- for fields $ \ (k, v) -> do
            addr <- alloc k
            (k, v) <$ assign addr v
          -- FIXME: should records reference types by address instead?
          pure (Alg (Record (Map.fromList fields')))
        _ ... m = pure (Just m)


data Constraint name = Type name :===: Type name
  deriving (Eq, Ord, Show)

infix 4 :===:

data Solution name
  = Int := Type name
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: (Carrier sig m, Member Fresh sig) => m (Type name)
meta = pure <$> Fresh.fresh

unify :: (Carrier sig m, Member (State (Set.Set (Constraint name))) sig, Ord name) => Type name -> Type name -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1 :===: t2))

type Substitution name = IntMap.IntMap (Type name)

solve :: (Member (State (Substitution name)) sig, MonadFail m, Ord name, Show name, Carrier sig m) => Set.Set (Constraint name) -> m ()
solve cs = for_ cs solve
  where solve = \case
          -- FIXME: how do we enforce proper subtyping? row polymorphism or something?
          Alg (Record f1) :===: Alg (Record f2) -> traverse solve (Map.intersectionWith (:===:) f1 f2) $> ()
          Alg (Arr a1 b1) :===: Alg (Arr a2 b2) -> solve (a1 :===: a2) *> solve (b1 :===: b2)
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
