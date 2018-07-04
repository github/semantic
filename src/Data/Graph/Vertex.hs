{-# LANGUAGE DeriveAnyClass #-}
module Data.Graph.Vertex
  ( Vertex (..)
  , moduleVertex
  , packageVertex
  , vertexToType
  ) where

import Prologue hiding (packageName)

import           Data.Aeson
import qualified Data.Text as T

import Data.Abstract.Module (ModuleInfo (..))
import Data.Abstract.Name
import Data.Abstract.Package (PackageInfo (..))

-- | A vertex of some specific type.
data Vertex
  = Package  { vertexName :: Text }
  | Module   { vertexName :: Text }
  | Variable { vertexName :: Text }
  deriving (Eq, Ord, Show, Generic, Hashable)

packageVertex :: PackageInfo -> Vertex
packageVertex = Package . formatName . packageName

moduleVertex :: ModuleInfo -> Vertex
moduleVertex = Module . T.pack . modulePath

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexName v, "type" .= vertexToType v ]

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"
