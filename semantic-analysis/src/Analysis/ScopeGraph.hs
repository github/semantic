{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, TypeApplications, TypeOperators #-}
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
import           Analysis.Name
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fail.WithLoc
import           Control.Effect.Carrier
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
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
  :: Ord (term Name)
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term Name Name (ScopeGraph Name) m
     -> (term Name -> m (ScopeGraph Name))
     -> (term Name -> m (ScopeGraph Name))
     )
  -> [File (term Name)]
  -> (Heap (ScopeGraph Name), [File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name))])
scopeGraph eval
  = run
  . runFresh
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap (ScopeGraph Name))) sig
     , Ord (term Name)
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Path.AbsRelFile) sig, Member (Reader Span) sig, MonadFail m)
     => Analysis term Name Name (ScopeGraph Name) m
     -> (term Name -> m (ScopeGraph Name))
     -> (term Name -> m (ScopeGraph Name))
     )
  -> File (term Name)
  -> m (File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name)))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv @Name
            . runReader (Map.empty @Name @Ref)
            . runFail
            . fmap fold
            . convergeTerm (A.runHeap @Name @(ScopeGraph Name) . fix (cacheTerm . eval scopeGraphAnalysis))

scopeGraphAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member (Env Name Name) sig
     , Member (A.Heap Name (ScopeGraph Name)) sig
     , Member (Reader Path.AbsRelFile) sig
     , Member (Reader Span) sig
     )
  => Analysis term Name Name (ScopeGraph Name) m
scopeGraphAnalysis = Analysis{..}
  where abstract eval name body = do
          addr <- alloc @Name @Name name
          A.assign @Name @(ScopeGraph Name) name mempty
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
            (k, v') <$ A.assign @Name addr v'
          pure (foldMap snd fields')
        _ ... m = pure (Just m)
