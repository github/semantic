{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Graph.Vertex where

import Data.Abstract.Declarations
import           Data.Abstract.Module (ModuleInfo (..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo (..))
import           Data.Aeson
import           Data.Span
import           Data.Record
import           Data.Term
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax as Syntax
import qualified Data.Text as T
import           Prologue hiding (packageName)

-- | A vertex of some specific type.
data Vertex
  = Package  { packageName :: Text }
  | Module   { moduleName :: Text }
  | Variable { variableName :: Text, variableModuleName :: Text, variableSpan :: Span }
  | Method   { methodName :: Text, methodModuleName :: Text, methodSpan :: Span }
  | Function { functionName :: Text, functionModuleName :: Text, functionSpan :: Span }
  deriving (Eq, Ord, Show, Generic, Hashable)

packageVertex :: PackageInfo -> Vertex
packageVertex (PackageInfo name _) = Package (formatName name)

moduleVertex :: ModuleInfo -> Vertex
moduleVertex = Module . T.pack . modulePath

variableVertex :: Text -> ModuleInfo -> Span -> Vertex
variableVertex name ModuleInfo{..} = Variable name (T.pack modulePath)

methodVertex :: Text -> ModuleInfo -> Span -> Vertex
methodVertex name ModuleInfo{..} = Method name (T.pack modulePath)

functionVertex :: Text -> ModuleInfo -> Span -> Vertex
functionVertex name ModuleInfo{..} = Function name (T.pack modulePath)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexName v, "type" .= vertexToType v ]

vertexName :: Vertex -> Text
vertexName Package{..} = packageName <> " (Package)"
vertexName Module{..} = moduleName <> " (Module)"
vertexName Variable{..} = "[" <> variableModuleName <> "]." <> variableName <> " (Variable" <> " " <> showSpan variableSpan <> ")"
vertexName Method{..} = "[" <> methodModuleName <> "]." <> methodName <> " (Method" <> " " <> showSpan methodSpan <> ")"
vertexName Function{..} = "[" <> functionModuleName <> "]." <> functionName <> " (Function" <> " " <> showSpan functionSpan <> ")"

showSpan :: Span -> Text
showSpan (Span (Pos a b) (Pos c d)) = T.pack $
  show a <> "," <> show b
  <> " - "
  <> show c <> "," <> show d

vertexToType :: Vertex -> Text
vertexToType Package{}  = "package"
vertexToType Module{}   = "module"
vertexToType Variable{} = "variable"
vertexToType Method{}   = "method"
vertexToType Function{} = "function"


class VertexDeclaration syntax where
  toVertex :: (Declarations1 syntax, Foldable syntax, HasField fields Span)
           => Record fields
           -> ModuleInfo
           -> syntax (Term syntax a)
           -> Maybe (Vertex, Name)

instance (VertexDeclaration' syntax syntax) => VertexDeclaration syntax where
  toVertex = toVertex'

class VertexDeclaration' whole syntax where
  toVertex' :: (Declarations1 whole, Foldable whole, HasField fields Span)
            => Record fields
            -> ModuleInfo
            -> syntax (Term whole a)
            -> Maybe (Vertex, Name)

instance (VertexDeclarationStrategy syntax ~ strategy, VertexDeclarationWithStrategy strategy whole syntax) => VertexDeclaration' whole syntax where
  toVertex' = toVertexWithStrategy (Proxy :: Proxy strategy)

data Strategy = Default | Custom

type family VertexDeclarationStrategy syntax where
  VertexDeclarationStrategy Syntax.Identifier = 'Custom
  VertexDeclarationStrategy Declaration.Function = 'Custom
  VertexDeclarationStrategy Declaration.Method = 'Custom
  VertexDeclarationStrategy (Sum _) = 'Custom
  VertexDeclarationStrategy syntax  = 'Default

class VertexDeclarationWithStrategy (strategy :: Strategy) whole syntax where
  toVertexWithStrategy :: (Declarations1 whole, Foldable whole, HasField fields Span)
                       => proxy strategy
                       -> Record fields
                       -> ModuleInfo
                       -> syntax (Term whole a)
                       -> Maybe (Vertex, Name)

-- | The 'Default' strategy produces 'Nothing'.
instance VertexDeclarationWithStrategy 'Default whole syntax where
  toVertexWithStrategy _ _ _ _ = Nothing

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomVertexDeclaration' instance for the type.
instance CustomVertexDeclaration whole syntax => VertexDeclarationWithStrategy 'Custom whole syntax where
  toVertexWithStrategy _ = customToVertex

class CustomVertexDeclaration whole syntax where
  customToVertex :: (Declarations1 whole, Foldable whole, HasField fields Span)
                 => Record fields
                 -> ModuleInfo
                 -> syntax (Term whole a)
                 -> Maybe (Vertex, Name)

instance Apply (VertexDeclaration' whole) fs => CustomVertexDeclaration whole (Sum fs) where
  customToVertex ann info = apply @(VertexDeclaration' whole) (toVertex' ann info)

instance CustomVertexDeclaration whole Syntax.Identifier where
  customToVertex ann info (Syntax.Identifier name) = Just (variableVertex (formatName name) info (getField ann), name)

instance CustomVertexDeclaration whole Declaration.Function where
  customToVertex ann info term@Declaration.Function{} = (\n -> (functionVertex (formatName n) info (getField ann), n)) <$> liftDeclaredName declaredName term

instance CustomVertexDeclaration whole Declaration.Method where
  customToVertex ann info term@Declaration.Method{} = (\n -> (methodVertex (formatName n) info (getField ann), n)) <$> liftDeclaredName declaredName term
