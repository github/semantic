{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, TypeApplications, TypeOperators #-}
module Analysis.ScopeGraph
( ScopeGraph(..)
, Ref (..)
, Decl(..)
, scopeGraph
, scopeGraphAnalysis
) where

import           Analysis.Eval
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Effect.Carrier
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad ((>=>))
import           Data.File
import           Data.Foldable (fold)
import           Data.Function (fix)
import           Data.List.NonEmpty
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import           Data.Proxy
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Prelude hiding (fail)

data Decl = Decl
  { declSymbol :: Name
  , declLoc    :: Loc
  }
  deriving (Eq, Ord, Show)

newtype Ref = Ref Loc
  deriving (Eq, Ord, Show)

newtype ScopeGraph = ScopeGraph { unScopeGraph :: Map.Map Decl (Set.Set Ref) }
  deriving (Eq, Ord, Show)

instance Semigroup ScopeGraph where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

instance Monoid ScopeGraph where
  mempty = ScopeGraph Map.empty

scopeGraph
  :: Ord term
  => (forall sig m
     .  (Carrier sig m, Member (Reader Loc) sig, MonadFail m)
     => Analysis term Name ScopeGraph m
     -> (term -> m ScopeGraph)
     -> (term -> m ScopeGraph)
     )
  -> [File term]
  -> (Heap Name ScopeGraph, [File (Either (Loc, String) ScopeGraph)])
scopeGraph eval
  = run
  . runFresh
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap Name ScopeGraph)) sig
     , Ord term
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Loc) sig, MonadFail m)
     => Analysis term Name ScopeGraph m
     -> (term -> m ScopeGraph)
     -> (term -> m ScopeGraph)
     )
  -> File term
  -> m (File (Either (Loc, String) ScopeGraph))
runFile eval file = traverse run file
  where run = runReader (fileLoc file)
            . runReader (Map.empty @Name @Loc)
            . runFailWithLoc
            . fmap fold
            . convergeTerm (Proxy @Name) (fix (cacheTerm . eval scopeGraphAnalysis))

scopeGraphAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member (Reader Loc) sig
     , Member (Reader (Map.Map Name Loc)) sig
     , Member (State (Heap Name ScopeGraph)) sig
     )
  => Analysis term Name ScopeGraph m
scopeGraphAnalysis = Analysis{..}
  where alloc = pure
        bind name _ m = do
          loc <- ask @Loc
          local (Map.insert name loc) m
        lookupEnv = pure . Just
        deref addr = do
          ref <- asks Ref
          bindLoc <- asks (Map.lookup addr)
          cell <- gets (Map.lookup addr >=> nonEmpty . Set.toList)
          let extending = mappend (extendBinding addr ref bindLoc)
          maybe (pure Nothing) (foldMapA (pure . Just . extending)) cell
        assign addr v = do
          ref <- asks Ref
          bindLoc <- asks (Map.lookup addr)
          modify (Map.insertWith (<>) addr (Set.singleton (extendBinding addr ref bindLoc <> v)))
        abstract eval name body = do
          addr <- alloc name
          assign name (mempty @ScopeGraph)
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
            loc <- ask @Loc
            let v' = ScopeGraph (Map.singleton (Decl k loc) mempty) <> v
            (k, v') <$ assign addr v'
          pure (foldMap snd fields')
        _ ... m = pure (Just m)

        extendBinding addr ref bindLoc = ScopeGraph (maybe Map.empty (\ bindLoc -> Map.singleton (Decl addr bindLoc) (Set.singleton ref)) bindLoc)
