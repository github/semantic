{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Stack.File
  ( File (..)
  , path_
  , language_
  , nodes_
  , paths_
  , edgesInFile
  , partialGraphOfFile
  ) where

import qualified Algebra.Graph.Labelled as Graph
import           Control.Lens.Getter
import           Data.Foldable (toList)
import           Data.Generics.Product
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import           Stack.Edge
import           Stack.Node (Node)
import           Stack.Path (Path, edgeLabels_, endingNode_, startingNode_)
import qualified System.Path as Path

-- | A native representation of the stack-graph file type, bridged to
-- the StackGraphFile type defined in the protobufs.
data File = File
  { path     :: Path.AbsRelFile
  , language :: Text
  , nodes    :: Vector Node
  , paths    :: Vector Path
  , errors   :: Vector Text
  } deriving (Eq, Show, Generic)

path_ :: Lens' File Path.AbsRelFile
path_ = field @"path"

language_ :: Lens' File Text
language_ = field @"language"

nodes_ :: Lens' File (Vector Node)
nodes_ = field @"nodes"

paths_ :: Lens' File (Vector Path)
paths_ = field @"paths"

-- | Compute the set of all edges in a file, as computed by walking all the paths.
edgesInFile :: File -> Set Edge
edgesInFile = foldMap edgesOf . paths
  where
    edgesOf :: Path -> Set Edge
    edgesOf p = Set.singleton (Edge (p ^. startingNode_) (p ^. endingNode_) (T.take 1 (p ^. edgeLabels_)))

-- | Compute an algebraic labelled graph from the edges present in a given file.
partialGraphOfFile :: File -> Graph.Graph Text Node
partialGraphOfFile = Graph.edges . toList . Set.map tupled . edgesInFile
  where
    tupled (Edge a b c) = (c, a, b)

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

