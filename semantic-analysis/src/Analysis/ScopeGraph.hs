{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Analysis.ScopeGraph
( ScopeGraph(..)
, Ref (..)
, Decl(..)
, scopeGraph
, scopeGraphAnalysis
) where

import           Analysis.Analysis
import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Effect.Carrier
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Prelude hiding (fail)
import           Source.Span
import qualified System.Path as Path

data Decl name = Decl
  { declSymbol :: name
  , declPath   :: Path.AbsRelFile
  , declSpan   :: Span
  }
  deriving (Eq, Ord, Show)

data Ref = Ref
  { refPath :: Path.AbsRelFile
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)

newtype ScopeGraph name = ScopeGraph { unScopeGraph :: Map.Map (Decl name) (Set.Set Ref) }
  deriving (Eq, Ord, Show)

instance Ord name => Semigroup (ScopeGraph name) where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

instance Ord name => Monoid (ScopeGraph name) where
  mempty = ScopeGraph Map.empty

scopeGraph
  :: (Ord name, Ord (term name))
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term name name (ScopeGraph name) m
     -> (term name -> m (ScopeGraph name))
     -> (term name -> m (ScopeGraph name))
     )
  -> [File (term name)]
  -> (Heap name (ScopeGraph name), [File (Either (Path.AbsRelFile, Span, String) (ScopeGraph name))])
scopeGraph eval
  = run
  . runFresh
  . runHeap
  . traverse (runFile eval)

runFile
  :: forall term name m sig
  .  ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap name (ScopeGraph name))) sig
     , Ord name
     , Ord (term name)
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term name name (ScopeGraph name) m
     -> (term name -> m (ScopeGraph name))
     -> (term name -> m (ScopeGraph name))
     )
  -> File (term name)
  -> m (File (Either (Path.AbsRelFile, Span, String) (ScopeGraph name)))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv @name
            . runReader (Map.empty @name @Ref)
            . runFail
            . fmap fold
            . convergeTerm (Proxy @name) (A.runHeap @name @(ScopeGraph name) . fix (cacheTerm . eval scopeGraphAnalysis))

scopeGraphAnalysis
  :: forall term name m sig
  .  ( Alternative m
     , Carrier sig m
     , Member (Env name name) sig
     , Member (A.Heap name (ScopeGraph name)) sig
     , Member (Reader Path.AbsRelFile) sig
     , Member (Reader Span) sig
     , Ord name
     )
  => Analysis term name name (ScopeGraph name) m
scopeGraphAnalysis = Analysis{..}
  where abstract eval name body = do
          addr <- alloc @name @name name
          A.assign @name @(ScopeGraph name) name mempty
          bind name addr (eval body)
        apply _ f a = pure (f <> a)
        unit = pure mempty
        bool _ = pure mempty
        asBool _ = pure True <|> pure False
        string _ = pure mempty
        asString _ = pure mempty
        record fields = do
          fields' <- for fields $ \ (k, v) -> do
            addr <- alloc k
            path <- ask
            span <- ask
            let v' = ScopeGraph (Map.singleton (Decl k path span) mempty) <> v
            (k, v') <$ A.assign @name addr v'
          pure (foldMap snd fields')
        _ ... m = pure (Just m)
