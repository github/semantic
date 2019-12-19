{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, LambdaCase, QuantifiedConstraints, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Analysis.ImportGraph
( ImportGraph
, importGraph
) where

import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import qualified Analysis.Effect.Domain as A
import           Analysis.File
import qualified Analysis.Intro as I
import           Analysis.FlowInsensitive
import           Analysis.Name
import           Control.Algebra
import           Control.Applicative (Alternative(..))
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Monad.Trans.Class
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Traversable (for)
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Scope (Scope)
import qualified System.Path as Path

type ImportGraph = Map.Map Text (Set.Set Text)

type Addr = Name

data Value semi = Value
  { valueSemi  :: semi
  , valueGraph :: ImportGraph
  }
  deriving (Eq, Ord, Show)

instance Semigroup (Value (Semi term)) where
  Value s1 g1 <> Value s2 g2 = Value (s1 <> s2) (Map.unionWith (<>) g1 g2)

instance Monoid (Value (Semi term)) where
  mempty = Value mempty mempty

data Semi term
  = Bottom
  | Closure Path.AbsRelFile Span (Named (Scope () term Addr))
  | Unit
  | Bool
  -- FIXME: Bound String values.
  | String Text
  | Top

instance Semigroup (Semi term) where
  s1        <> Bottom    = s1
  Bottom    <> s2        = s2
  Unit      <> Unit      = Unit
  Bool      <> Bool      = Bool
  String s1 <> String s2
    | s1 == s2           = String s1
  _         <> _         = Top

instance Monoid (Semi term) where
  mempty = Bottom

deriving instance ( forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Semi f)
deriving instance ( forall a . Eq   a => Eq   (f a)
                  , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Semi f)
deriving instance ( forall a . Show a => Show (f a))          => Show (Semi f)


importGraph
  :: ( Monad term
     , forall a . Eq  a => Eq  (term a)
     , forall a . Ord a => Ord (term a)
     )
  => (forall sig m
     .  (Has (A.Domain term Addr (Value (Semi term)) :+: Env Addr :+: A.Heap Addr (Value (Semi term)) :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m (Value (Semi term)))
     -> (term Addr -> m (Value (Semi term)))
     )
  -> [File (term Addr)]
  -> ( Heap (Value (Semi term))
     , [File (Either (Path.AbsRelFile, Span, String) (Value (Semi term)))]
     )
importGraph eval
  = run
  . evalFresh 0
  . runHeap
  . traverse (runFile eval)

runFile
  :: forall term m sig
  .  ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap (Value (Semi term)))) sig m
     , Monad term
     , forall a . Eq  a => Eq  (term a)
     , forall a . Ord a => Ord (term a)
     )
  => (forall sig m
     .  (Has (A.Domain term Addr (Value (Semi term)) :+: Env Addr :+: A.Heap Addr (Value (Semi term)) :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m (Value (Semi term)))
     -> (term Addr -> m (Value (Semi term)))
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) (Value (Semi term))))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv
            . runFail
            . fmap fold
            . convergeTerm 0 (A.runHeap @Addr @(Value (Semi term)) . fix (\ eval' -> runDomain eval' . fix (cacheTerm . eval)))


runDomain :: (term Addr -> m (Value (Semi term))) -> DomainC term m a -> m a
runDomain eval (DomainC m) = runReader eval m

newtype DomainC term m a = DomainC (ReaderC (term Addr -> m (Value (Semi term))) m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

instance MonadTrans (DomainC term) where
  lift = DomainC . lift

-- FIXME: decompose into a product domain and two atomic domains
instance (Alternative m, Has (Env Addr :+: A.Heap Addr (Value (Semi term)) :+: Reader Path.AbsRelFile :+: Reader Span) sig m) => Algebra (A.Domain term Addr (Value (Semi term)) :+: sig) (DomainC term m) where
  alg = \case
    L (A.Abstract i k) -> case i of
      I.Unit     -> k (Value Unit mempty)
      I.Bool _   -> k (Value Bool mempty)
      I.String s -> k (Value (String s) mempty)
      I.Lam b    -> do
        path <- ask
        span <- ask
        k (Value (Closure path span b) mempty)
      I.Record f -> do
        eval <- DomainC ask
        fields <- for f $ \ (k, t) -> do
          addr <- alloc @Addr k
          v <- lift (eval t)
          v <$ A.assign @Addr @(Value (Semi term)) addr v
        k (fold fields)
    L (A.Concretize (Value s _) k) -> case s of
      Bottom        -> k I.Unit -- FIXME: what should we do when we don’t know anything about the value?
      Unit          -> k I.Unit
      Bool          -> k (I.Bool True) <|> k (I.Bool False)
      String s      -> k (I.String s)
      Closure _ _ b -> k (I.Lam b)
      Top           -> k I.Unit -- FIXME: what should we do when the value comprised multiple things?
    R other -> DomainC (send (handleCoercible other))
