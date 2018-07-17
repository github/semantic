{-# LANGUAGE DeriveAnyClass #-}
module Data.Graph.Vertex
  ( Vertex (..)
  , moduleVertex
  , packageVertex
  , variableVertex
  , vertexName
  , vertexToType
  ) where

import           Data.Abstract.Module (ModuleInfo (..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo (..))
import           Data.Aeson
import           Data.Span
import qualified Data.Text as T
import           Prologue hiding (packageName)

-- | A vertex of some specific type.
data Vertex
  = Package  { packageName :: Text }
  | Module   { moduleName :: Text }
  | Variable { variableName :: Text, variableModuleName :: Text, variableSpan :: Span }
  deriving (Eq, Ord, Show, Generic, Hashable)

packageVertex :: PackageInfo -> Vertex
packageVertex (PackageInfo name _) = Package (formatName name)

moduleVertex :: ModuleInfo -> Vertex
moduleVertex = Module . T.pack . modulePath

variableVertex :: Text -> ModuleInfo -> Span -> Vertex
variableVertex name ModuleInfo{..} = Variable name (T.pack modulePath)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexName v, "type" .= vertexToType v ]

vertexName :: Vertex -> Text
vertexName Package{..} = packageName <> " (P)"
vertexName Module{..} = moduleName <> " (M)"
vertexName Variable{..} = "[" <> variableModuleName <> "]." <> variableName <> " (V" <> " " <> showSpan variableSpan <> ")"
  where showSpan (Span (Pos a b) (Pos c d)) = T.pack $  show a <> "," <> show b
                                                     <> " - "
                                                     <> show c <> "," <> show d

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"
