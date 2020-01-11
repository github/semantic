{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.ScopeGraph
  ( ToScopeGraph(..)
  , ScopeGraph(..)
  , Info (..)
  , runScopeGraph
  , module GC
  , Addressable (..)
  ) where

import qualified Algebra.Graph
import           Algebra.Graph.Class as GC
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import           Data.Text (Text, unpack)
import           Data.Unique
import           Source.Loc (Loc (..))
import           Source.Source (Source)
import qualified System.Path as Path

data Node a = Node
  { contents :: a
  } deriving (Eq, Ord)

instance Show a => Show (Node a) where
  show = show . contents


newtype ScopeGraph a = ScopeGraph (Algebra.Graph.Graph a)
  deriving (Show, Eq)

instance Semigroup (ScopeGraph a) where (<>) = GC.overlay
instance Monoid (ScopeGraph a) where mempty = GC.empty

-- ref :: Text -> IO (Vertex (ScopeGraph Info))
-- ref t = Node <$> (Ref <$> newUnique <*> pure t)

-- scope :: IO (Vertex (ScopeGraph Info))
-- scope = Node . Scope <$> newUnique


instance GC.Graph (ScopeGraph a) where
  type Vertex (ScopeGraph a) = a
  empty  = ScopeGraph GC.empty
  vertex = ScopeGraph . GC.vertex
  overlay (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `GC.overlay` b)
  connect (ScopeGraph a) (ScopeGraph b) = ScopeGraph (a `GC.connect` b)

data Info = Decl Int Text
          | Scope Int
          | Root Path.AbsRelFile
  deriving (Eq, Ord)

class Addressable a where
  scope :: Int -> a
  decl  :: Int -> Text -> a
  root  :: Path.AbsRelFile -> a

instance Addressable Info where
  scope = Scope
  decl  = Decl
  root  = Root

instance Show Info where
  show = \case
    Decl _ i -> unpack i
    Scope u -> "❇️  " <> show u
    Root _  -> "🏁"

class ToScopeGraph t where
  scopeGraph ::
    ( Has (Reader Source) sig m
    , Has (Reader (Vertex (ScopeGraph Info))) sig m
    , MonadIO m
    )
    => t Loc
    -> m (ScopeGraph Info)

-- instance ToScopeGraph Py.Identifier where
--   scopeGraph _ (Py.Identifier _ t) = ScopeGraph . G.vertex . Node (Ref t) <$> liftIO newUnique

runScopeGraph :: ToScopeGraph t => Path.AbsRelFile -> Source -> t Loc -> IO (ScopeGraph Info)
runScopeGraph p src item = do
  runM . runReader (Root p) . runReader src $ scopeGraph item
