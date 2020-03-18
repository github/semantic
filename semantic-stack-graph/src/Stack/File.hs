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
  ) where

import           Data.Generics.Product
import           Data.Text (Text)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import           Stack.Node (Node)
import           Stack.Path (Path)
import qualified System.Path as Path

type ParseError = Text

data File = File
  { path     :: Path.AbsRelFile
  , language :: Text
  , nodes    :: Vector Node
  , paths    :: Vector Path
  , errors   :: Vector ParseError
  } deriving (Eq, Show, Generic)

path_ :: Lens' File Path.AbsRelFile
path_ = field @"path"

language_ :: Lens' File Text
language_ = field @"language"

nodes_ :: Lens' File (Vector Node)
nodes_ = field @"nodes"

paths_ :: Lens' File (Vector Path)
paths_ = field @"paths"

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
