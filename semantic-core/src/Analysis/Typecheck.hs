{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, OverloadedStrings, QuantifiedConstraints, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}
module Analysis.Typecheck
( Monotype (..)
, Meta
, Polytype (PForAll, PBool, PArr)
, Scope
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
import           Data.Name as Name
import           Data.Scope
import qualified Data.Set as Set
import           Data.Stack
import           Data.Term
import           GHC.Generics (Generic1)
import           Prelude hiding (fail)

data Monotype a
  = MBool
  | MUnit
  | MString
  | MMeta a
  | MArr (Monotype a) (Monotype a)
  | MRecord (Map.Map User (Monotype a))
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

type Meta = Int

data Polytype f a
  = PForAll (Scope () f a)
  | PUnit
  | PBool
  | PString
  | PArr (f a) (f a)
  | PRecord (Map.Map User (f a))
  deriving (Foldable, Functor, Generic1, Traversable)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Polytype f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Polytype f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Polytype f a)

instance HFunctor Polytype
instance RightModule Polytype where
  PForAll b >>=* f = PForAll (b >>=* f)
  PUnit     >>=* _ = PUnit
  PBool     >>=* _ = PBool
  PString   >>=* _ = PString
  PArr a b  >>=* f = PArr (a >>= f) (b >>= f)
  PRecord m >>=* f = PRecord ((>>= f) <$> m)


forAll :: (Eq a, Carrier sig m, Member Polytype sig) => a -> m a -> m a
forAll n body = send (PForAll (Data.Scope.bind1 n body))

forAlls :: (Eq a, Carrier sig m, Member Polytype sig, Foldable t) => t a -> m a -> m a
forAlls ns body = foldr forAll body ns

generalize :: (Carrier sig m, Member Naming sig) => Monotype Meta -> m (Term Polytype Gensym)
generalize ty = namespace "generalize" $ do
  Gensym root _ <- Name.fresh
  pure (forAlls (map (Gensym root) (IntSet.toList (mvs ty))) (fold root ty))
  where fold root = \case
          MUnit      -> Term PUnit
          MBool      -> Term PBool
          MString    -> Term PString
          MMeta i    -> Var (Gensym root i)
          MArr a b   -> Term (PArr (fold root a) (fold root b))
          MRecord fs -> Term (PRecord (fold root <$> fs))


typecheckingFlowInsensitive :: [File (Term Core.Core Name)] -> (Heap Name (Monotype Meta), [File (Either (Loc, String) (Term Polytype Gensym))])
typecheckingFlowInsensitive
  = run
  . runFresh
  . runNaming
  . runHeap (Gen (Gensym (Nil :> "root") 0))
  . (>>= traverse (traverse (traverse generalize)))
  . traverse runFile

runFile :: ( Carrier sig m
           , Effect sig
           , Member Fresh sig
           , Member Naming sig
           , Member (State (Heap Name (Monotype Meta))) sig
           )
        => File (Term Core.Core Name)
        -> m (File (Either (Loc, String) (Monotype Meta)))
runFile file = traverse run file
  where run
          = (\ m -> do
              (subst, t) <- m
              modify @(Heap Name (Monotype Meta)) (substAll subst)
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

typecheckingAnalysis :: (Alternative m, Carrier sig m, Member Fresh sig, Member (State (Set.Set Constraint)) sig, Member (State (Heap Name (Monotype Meta))) sig, MonadFail m) => Analysis Name (Monotype Meta) m
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
          pure (MArr arg ty)
        apply _ f a = do
          _A <- meta
          _B <- meta
          unify (MArr _A _B) f
          unify _A a
          pure _B
        unit = pure MUnit
        bool _ = pure MBool
        asBool b = unify MBool b >> pure True <|> pure False
        string _ = pure MString
        asString s = unify MString s $> mempty
        frame = fail "unimplemented"
        edge _ _ = pure ()
        _ ... m = m


data Constraint = Monotype Meta :===: Monotype Meta
  deriving (Eq, Ord, Show)

infix 4 :===:

data Solution
  = Int := Monotype Meta
  deriving (Eq, Ord, Show)

infix 5 :=

meta :: (Carrier sig m, Member Fresh sig) => m (Monotype Meta)
meta = MMeta <$> Fresh.fresh

unify :: (Carrier sig m, Member (State (Set.Set Constraint)) sig) => Monotype Meta -> Monotype Meta -> m ()
unify t1 t2
  | t1 == t2  = pure ()
  | otherwise = modify (<> Set.singleton (t1 :===: t2))

type Substitution = IntMap.IntMap (Monotype Meta)

solve :: (Carrier sig m, Member (State Substitution) sig, MonadFail m) => Set.Set Constraint -> m ()
solve cs = for_ cs solve
  where solve = \case
          -- FIXME: how do we enforce proper subtyping? row polymorphism or something?
          MRecord f1 :===: MRecord f2 -> traverse solve (Map.intersectionWith (:===:) f1 f2) $> ()
          MArr a1 b1 :===: MArr a2 b2 -> solve (a1 :===: a2) *> solve (b1 :===: b2)
          MMeta m1   :===: MMeta m2   | m1 == m2 -> pure ()
          MMeta m1   :===: t2         -> do
            sol <- solution m1
            case sol of
              Just (_ := t1) -> solve (t1 :===: t2)
              Nothing | m1 `IntSet.member` mvs t2 -> fail ("Occurs check failure: " <> show m1 <> " :===: " <> show t2)
                      | otherwise                 -> modify (IntMap.insert m1 t2 . subst (m1 := t2))
          t1         :===: MMeta m2   -> solve (MMeta m2 :===: t1)
          t1         :===: t2         -> unless (t1 == t2) $ fail ("Type mismatch:\nexpected: " <> show t1 <> "\n  actual: " <> show t2)

        solution m = fmap (m :=) <$> gets (IntMap.lookup m)

substAll :: Substitutable t => Substitution -> t -> t
substAll s a = foldl' (flip subst) a (map (uncurry (:=)) (IntMap.toList s))


class FreeVariables t where
  mvs :: t -> IntSet.IntSet

instance FreeVariables (Monotype Meta) where
  mvs MUnit        = mempty
  mvs MBool        = mempty
  mvs MString      = mempty
  mvs (MArr a b)   = mvs a <> mvs b
  mvs (MMeta m)    = IntSet.singleton m
  mvs (MRecord fs) = foldMap mvs fs

instance FreeVariables Constraint where
  mvs (t1 :===: t2) = mvs t1 <> mvs t2

class Substitutable t where
  subst :: Solution -> t -> t

instance Substitutable (Monotype Meta) where
  subst s con = case con of
    MUnit         -> MUnit
    MBool         -> MBool
    MString       -> MString
    MArr a b      -> MArr (subst s a) (subst s b)
    MMeta m'
      | m := t <- s
      , m == m'   -> t
      | otherwise -> MMeta m'
    MRecord fs    -> MRecord (subst s <$> fs)

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
