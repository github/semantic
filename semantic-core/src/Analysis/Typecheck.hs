{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedStrings, QuantifiedConstraints, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators #-}
module Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (..)
, typecheckingFlowInsensitive
, typecheckingAnalysis
) where

import           Analysis.Eval
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Effect.Carrier
import           Control.Effect.Fail
import           Control.Effect.Fresh as Fresh
import           Control.Effect.Reader hiding (Local)
import           Control.Effect.State
import           Control.Monad (unless)
import           Control.Monad.Module
import qualified Data.Core as Core
import           Data.File
import           Data.Foldable (foldl', for_)
import           Data.Function (fix)
import           Data.Functor (($>))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.List.NonEmpty (nonEmpty)
import           Data.Loc
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Name as Name
import           Data.Scope
import qualified Data.Set as Set
import           Data.Stack
import           Data.Term
import           GHC.Generics (Generic1)
import           Prelude hiding (fail)

data Monotype f a
  = Bool
  | Unit
  | String
  | Arr (f a) (f a)
  | Record (Map.Map User (f a))
  deriving (Foldable, Functor, Generic1, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Monotype f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Monotype f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Monotype f a)

instance HFunctor Monotype
instance RightModule Monotype where
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
forAll n body = send (PForAll (Data.Scope.bind1 n body))

forAlls :: (Eq a, Carrier sig m, Member Polytype sig, Foldable t) => t a -> m a -> m a
forAlls ns body = foldr forAll body ns

generalize :: Term Monotype Meta -> Term (Polytype :+: Monotype) Gensym
generalize ty = fromJust (closed (forAlls (IntSet.toList (mvs ty)) (hoistTerm R ty)))


typecheckingFlowInsensitive :: [File (Term Core.Core Name)] -> (Heap Name (Term Monotype Meta), [File (Either (Loc, String) (Term (Polytype :+: Monotype) Gensym))])
typecheckingFlowInsensitive
  = run
  . runFresh
  . runNaming
  . runHeap (Gen (Gensym (Nil :> "root") 0))
  . fmap (fmap (fmap (fmap generalize)))
  . traverse runFile

runFile :: ( Carrier sig m
           , Effect sig
           , Member Fresh sig
           , Member Naming sig
           , Member (State (Heap Name (Term Monotype Meta))) sig
           )
        => File (Term Core.Core Name)
        -> m (File (Either (Loc, String) (Term Monotype Meta)))
runFile file = traverse run file
  where run
          = (\ m -> do
              (subst, t) <- m
              modify @(Heap Name (Term Monotype Meta)) (substAll subst)
              pure (substAll subst <$> t))
          . runState (mempty :: Substitution)
          . runReader (fileLoc file)
          . runFailWithLoc
          . (\ m -> do
            (cs, t) <- m
            t <$ solve cs)
          . runState (Set.empty :: Set.Set Constraint)
          . (\ m -> do
              v <- meta
              bs <- m
              v <$ for_ bs (unify v))
          . convergeTerm (fix (cacheTerm . eval typecheckingAnalysis))

typecheckingAnalysis :: (Alternative m, Carrier sig m, Member Fresh sig, Member (State (Set.Set Constraint)) sig, Member (State (Heap Name (Term Monotype Meta))) sig, MonadFail m) => Analysis Name (Term Monotype Meta) m
typecheckingAnalysis = Analysis{..}
  where alloc = pure
        bind _ _ = pure ()
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr) >>= maybe (pure Nothing) (foldMapA (pure . Just)) . nonEmpty . maybe [] Set.toList
        assign addr ty = modify (Map.insertWith (<>) addr (Set.singleton ty))
        abstract eval name body = do
          -- FIXME: construct the associated scope
          addr <- alloc name
          arg <- meta
          assign addr arg
          ty <- eval body
          pure (Term (Arr arg ty))
        apply _ f a = do
          _A <- meta
          _B <- meta
          unify (Term (Arr _A _B)) f
          unify _A a
          pure _B
        unit = pure (Term Unit)
        bool _ = pure (Term Bool)
        asBool b = unify (Term Bool) b >> pure True <|> pure False
        string _ = pure (Term String)
        asString s = unify (Term String) s $> mempty
        frame = fail "unimplemented"
        edge _ _ = pure ()
        _ ... m = m


data Constraint = Term Monotype Meta :===: Term Monotype Meta
  deriving (Eq, Ord, Show)

infix 4 :===:

data Solution
  = Int := Term Monotype Meta
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: (Carrier sig m, Member Fresh sig) => m (Term Monotype Meta)
meta = pure <$> Fresh.fresh

unify :: (Carrier sig m, Member (State (Set.Set Constraint)) sig) => Term Monotype Meta -> Term Monotype Meta -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1 :===: t2))

type Substitution = IntMap.IntMap (Term Monotype Meta)

solve :: (Carrier sig m, Member (State Substitution) sig, MonadFail m) => Set.Set Constraint -> m ()
solve cs = for_ cs solve
  where solve = \case
          -- FIXME: how do we enforce proper subtyping? row polymorphism or something?
          Term (Record f1) :===: Term (Record f2) -> traverse solve (Map.intersectionWith (:===:) f1 f2) $> ()
          Term (Arr a1 b1) :===: Term (Arr a2 b2) -> solve (a1 :===: a2) *> solve (b1 :===: b2)
          Var m1   :===: Var m2   | m1 == m2 -> pure ()
          Var m1   :===: t2         -> do
            sol <- solution m1
            case sol of
              Just (_ := t1) -> solve (t1 :===: t2)
              Nothing | m1 `IntSet.member` mvs t2 -> fail ("Occurs check failure: " <> show m1 <> " :===: " <> show t2)
                      | otherwise                 -> modify (IntMap.insert m1 t2 . subst (m1 := t2))
          t1         :===: Var m2   -> solve (Var m2 :===: t1)
          t1         :===: t2         -> unless (t1 == t2) $ fail ("Type mismatch:\nexpected: " <> show t1 <> "\n  actual: " <> show t2)

        solution m = fmap (m :=) <$> gets (IntMap.lookup m)

substAll :: Substitutable t => Substitution -> t -> t
substAll s a = foldl' (flip subst) a (map (uncurry (:=)) (IntMap.toList s))


class FreeVariables t where
  mvs :: t -> IntSet.IntSet

instance FreeVariables (Term Monotype Meta) where
  mvs = foldMap IntSet.singleton

instance FreeVariables Constraint where
  mvs (t1 :===: t2) = mvs t1 <> mvs t2

class Substitutable t where
  subst :: Solution -> t -> t

instance Substitutable (Term Monotype Meta) where
  subst (i' := t') t = t >>= \ i -> if i == i' then t' else Var i

instance Substitutable Constraint where
  subst s (t1 :===: t2) = subst s t1 :===: subst s t2

instance Substitutable Solution where
  subst s (m := t) = m := subst s t

instance Substitutable a => Substitutable (IntMap.IntMap a) where
  subst s = IntMap.map (subst s)

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  subst s = Set.map (subst s)

instance Substitutable v => Substitutable (Map.Map k v) where
  subst s = fmap (subst s)
