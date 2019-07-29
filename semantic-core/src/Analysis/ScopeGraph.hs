{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, RecordWildCards, TypeApplications, TypeOperators #-}
module Analysis.ScopeGraph
( ScopeGraph
, Entry(..)
, scopeGraph
, scopeGraphAnalysis
) where

import           Analysis.Eval
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Effect.Carrier
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad ((>=>))
import qualified Data.Core as Core
import           Data.File
import           Data.Foldable (fold)
import           Data.Function (fix)
import           Data.List.NonEmpty
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import           Data.Proxy
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Term
import           Prelude hiding (fail)

data Entry = Entry
  { entrySymbol :: Text
  , entryLoc    :: Loc
  }
  deriving (Eq, Ord, Show)

newtype ScopeGraph = ScopeGraph { unScopeGraph :: Map.Map Entry (Set.Set Entry) }
  deriving (Eq, Monoid, Ord, Show)

instance Semigroup ScopeGraph where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

scopeGraph :: [File (Term (Core.Ann :+: Core.Core) User)] -> (Heap User ScopeGraph, [File (Either (Loc, String) ScopeGraph)])
scopeGraph
  = run
  . runFresh
  . runHeap
  . traverse runFile

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap User ScopeGraph)) sig
     )
  => File (Term (Core.Ann :+: Core.Core) User)
  -> m (File (Either (Loc, String) ScopeGraph))
runFile file = traverse run file
  where run = runReader (fileLoc file)
            . runReader (Map.empty @User @Loc)
            . runFailWithLoc
            . fmap fold
            . convergeTerm (Proxy @User) (fix (cacheTerm . eval scopeGraphAnalysis))

-- FIXME: decompose into a product domain and two atomic domains
scopeGraphAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member (Reader Loc) sig
     , Member (Reader (Map.Map User Loc)) sig
     , Member (State (Heap User ScopeGraph)) sig
     )
  => Analysis term User ScopeGraph m
scopeGraphAnalysis = Analysis{..}
  where alloc = pure
        bind name _ m = do
          loc <- ask @Loc
          local (Map.insert name loc) m
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr >=> nonEmpty . Set.toList) >>= maybe (pure Nothing) (foldMapA (pure . Just))
        assign addr ty = modify (Map.insertWith (<>) addr (Set.singleton ty))
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
        record fields = pure (foldMap snd fields)
        _ ... m = pure (Just m)
