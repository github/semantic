{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.ScopeGraph (ToScopeGraph(..), ScopeGraph(..), Info, runScopeGraph) where

import           Algebra.Graph (Graph)
import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as Graph.Class
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Function
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Unique
import           Source.Loc (Loc (..))
import           Source.Source as Source

data Node a = Node
  { contents :: a
  , ident    :: Data.Unique.Unique
  } deriving (Ord)

instance Eq (Node a) where
  (==) = (==) `on` ident

instance Show a => Show (Node a) where
  -- TODO: This is dropping ident field in Node
  show = show . contents


newtype ScopeGraph a = ScopeGraph (Graph (Node a))
  deriving (Show)

instance Graph.Class.Graph (ScopeGraph a) where
  type Vertex (ScopeGraph a) = Node a
  empty  = ScopeGraph G.empty
  vertex = ScopeGraph . G.vertex
  overlay (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `G.overlay` b)
  connect (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `G.connect` b)

data Info = Ref Text
          | Scope
          | Root
  deriving (Show)

type SGM =
  ( ReaderC (ScopeGraph Info)
  ( LiftC IO
  ))


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
  root <- ScopeGraph . G.vertex . Node Root <$> newUnique
  runM . runReader root . runReader src $ scopeGraph item
