{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.ScopeGraph
  ( ToScopeGraph(..)
  , ScopeGraph(..)
  , Info
  , runScopeGraph
  , scope
  , ref
  , root
  , Graph.Class.edges
  ) where

import           Algebra.Graph (Graph)
import qualified Algebra.Graph as G
import           Algebra.Graph.Class (Vertex)
import qualified Algebra.Graph.Class as Graph.Class
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Data.Text (Text, unpack)
import           Data.Unique
import           Source.Loc (Loc (..))
import           Source.Source (Source)

data Node a = Node
  { contents :: a
  } deriving (Eq, Ord)

instance Show a => Show (Node a) where
  show = show . contents


newtype ScopeGraph a = ScopeGraph (Graph (Node a))
  deriving (Show, Eq)

root :: Vertex (ScopeGraph Info)
root = Node Root

ref :: Text -> IO (Vertex (ScopeGraph Info))
ref t = Node <$> (Ref <$> newUnique <*> pure t)

scope :: IO (Vertex (ScopeGraph Info))
scope = Node . Scope <$> newUnique


instance Graph.Class.Graph (ScopeGraph a) where
  type Vertex (ScopeGraph a) = Node a
  empty  = ScopeGraph G.empty
  vertex = ScopeGraph . G.vertex
  overlay (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `G.overlay` b)
  connect (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `G.connect` b)

data Info = Ref Unique Text
          | Scope Unique
          | Root
  deriving (Eq, Ord)

instance Show Info where
  show = \case
    Ref _ i -> unpack i
    Scope u -> "❇️  " <> take 3 (show (hashUnique u))
    Root    -> "🏁"

class ToScopeGraph t where
  scopeGraph :: ( Has (Reader Source) sig m ) => t Loc -> m (ScopeGraph Info)

-- instance ToScopeGraph Py.Identifier where
--   scopeGraph _ (Py.Identifier _ t) = ScopeGraph . G.vertex . Node (Ref t) <$> liftIO newUnique

-- instance ToScopeGraph Py.Module where
--   scopeGraph src Py.Module { Py.extraChildren = stmts } = do
--     parent <- ask
--     self <- ScopeGraph . G.vertex . Node Scope <$> liftIO newUnique
--     foldr (\item acc -> do {
--               x <- acc;
--               y <- scopeGraph src item;
--               pure (x --> y);
--           }) (pure (parent --> self)) stmts

runScopeGraph :: ToScopeGraph t => Source -> t Loc -> IO (ScopeGraph Info)
runScopeGraph src item = do
  runM . runReader root . runReader src $ scopeGraph item
