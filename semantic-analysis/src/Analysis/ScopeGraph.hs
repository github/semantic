{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, OverloadedStrings, RankNTypes, RecordWildCards, TypeApplications, TypeOperators, UndecidableInstances #-}
module Analysis.ScopeGraph
( ScopeGraph(..)
, Ref (..)
, Decl(..)
, scopeGraph
) where

import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.Effect.Domain
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Intro
import           Analysis.Name
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Effect.State
import           Control.Monad.Trans.Class
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Scope
import qualified System.Path as Path

data Decl = Decl
  { declSymbol :: Name
  , declPath   :: Path.AbsRelFile
  , declSpan   :: Span
  }
  deriving (Eq, Ord, Show)

data Ref = Ref
  { refPath :: Path.AbsRelFile
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)

type Addr = Name

newtype ScopeGraph = ScopeGraph { unScopeGraph :: Map.Map Decl (Set.Set Ref) }
  deriving (Eq, Ord, Show)

instance Semigroup ScopeGraph where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

instance Monoid ScopeGraph where
  mempty = ScopeGraph Map.empty

scopeGraph
  :: (Monad term, Ord (term Addr))
  => (forall sig m
     .  (Has (Domain term Addr ScopeGraph :+: Env Addr :+: A.Heap Addr ScopeGraph :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m ScopeGraph)
     -> (term Addr -> m ScopeGraph)
     )
  -> [File (term Addr)]
  -> (Heap ScopeGraph, [File (Either (Path.AbsRelFile, Span, String) ScopeGraph)])
scopeGraph eval
  = run
  . evalFresh 0
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap ScopeGraph)) sig m
     , Monad term
     , Ord (term Addr)
     )
  => (forall sig m
     .  (Has (Domain term Addr ScopeGraph :+: Env Addr :+: A.Heap Addr ScopeGraph :+: Reader Path.AbsRelFile :+: Reader Span) sig m, MonadFail m)
     => (term Addr -> m ScopeGraph)
     -> (term Addr -> m ScopeGraph)
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) ScopeGraph))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv
            . runFail
            . fmap fold
            . convergeTerm 0 (A.runHeap @Addr @ScopeGraph . fix (\ eval' -> runDomain eval' . fix (cacheTerm . eval)))


runDomain :: (term Addr -> m ScopeGraph) -> DomainC term m a -> m a
runDomain eval (DomainC m) = runReader eval m

newtype DomainC term m a = DomainC (ReaderC (term Addr -> m ScopeGraph) m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

instance MonadTrans (DomainC term) where
  lift = DomainC . lift

instance (Alternative m, Has (Env Addr :+: A.Heap Addr ScopeGraph :+: Reader Path.AbsRelFile :+: Reader Span) sig m, Monad term) => Algebra (Domain term Addr ScopeGraph :+: sig) (DomainC term m) where
  alg = \case
    L (Abstract i k) -> case i of
      Unit -> k mempty
      Bool _ -> k mempty
      String _ -> k mempty
      Lam (Named n b) -> do
        eval <- DomainC ask
        addr <- alloc @Addr n
        A.assign @Addr @ScopeGraph addr mempty
        g <- bind n addr (lift (eval (instantiate1 (pure addr) b)))
        k g
      Record fields -> do
        eval <- DomainC ask
        fields' <- for fields $ \ (k, t) -> do
          addr <- alloc k
          path <- ask
          span <- ask
          v <- lift (eval t)
          let v' = ScopeGraph (Map.singleton (Decl k path span) mempty) <> v
          v' <$ A.assign @Addr addr v'
        k (fold fields')
    L (AsBool   _ k) -> k True <|> k False
    L (AsString _ k) -> k mempty
    L (AsLam    _ k) -> alloc (Name mempty) >>= k . Named (Name mempty) . lift . pure
    L (AsRecord _ k) -> k []
    R other -> DomainC (send (handleCoercible other))
