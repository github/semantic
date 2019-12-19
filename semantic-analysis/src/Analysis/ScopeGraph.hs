{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, TypeApplications, TypeOperators #-}
module Analysis.ScopeGraph
( ScopeGraph(..)
, Ref (..)
, Decl(..)
, scopeGraph
) where

import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Name
import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Effect.State
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
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

type Addr = Name

newtype ScopeGraph name = ScopeGraph { unScopeGraph :: Map.Map (Decl name) (Set.Set Ref) }
  deriving (Eq, Ord, Show)

instance Ord name => Semigroup (ScopeGraph name) where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

instance Ord name => Monoid (ScopeGraph name) where
  mempty = ScopeGraph Map.empty

scopeGraph
  :: Ord (term Addr)
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m (ScopeGraph Name))
     -> (term Addr -> m (ScopeGraph Name))
     )
  -> [File (term Addr)]
  -> (Heap (ScopeGraph Name), [File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name))])
scopeGraph eval
  = run
  . evalFresh 0
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap (ScopeGraph Name))) sig m
     , Ord (term Addr)
     )
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m (ScopeGraph Name))
     -> (term Addr -> m (ScopeGraph Name))
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name)))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv
            . runFail
            . fmap fold
            . convergeTerm 0 (A.runHeap @Addr @(ScopeGraph Name) . fix (cacheTerm . eval))

-- scopeGraphAnalysis
--   :: ( Alternative m
--      , Has (Env Name) sig m
--      , Has (A.Heap Name (ScopeGraph Name)) sig m
--      , Has (Reader Path.AbsRelFile) sig m
--      , Has (Reader Span) sig m
--      )
--   => Analysis Name (ScopeGraph Name) m
-- scopeGraphAnalysis = Analysis{..}
--   where -- abstract eval name body = do
--         --   addr <- alloc @Addr name
--         --   A.assign @Addr @(ScopeGraph Name) name mempty
--         --   bind name addr (eval body)
--         -- apply _ f a = pure (f <> a)
--         record fields = do
--           fields' <- for fields $ \ (k, v) -> do
--             addr <- alloc k
--             path <- ask
--             span <- ask
--             let v' = ScopeGraph (Map.singleton (Decl k path span) mempty) <> v
--             (k, v') <$ A.assign @Addr addr v'
--           pure (foldMap snd fields')
--         _ ... m = pure (Just m)
