{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Graph.Vertex
( Vertex (..)
, packageVertex
, moduleVertex
, unknownModuleVertex
, variableVertex
, methodVertex
, functionVertex
, vertexIdentifier
, showSpan
, VertexDeclaration (..)
, VertexDeclaration' (..)
, VertexDeclarationStrategy
, VertexDeclarationWithStrategy
) where

import           Data.Abstract.Declarations
import           Data.Abstract.Module (ModuleInfo (..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo (..))
import           Data.Aeson
import           Data.Record
import           Data.Span
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import           Data.Term
import qualified Data.Text as T
import           Prologue hiding (packageName)

-- | A vertex of some specific type.
data Vertex
  = Package       { vertexName :: Text }
  | Module        { vertexName :: Text }
  | UnknownModule { vertexName :: Text }
  | Variable      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Method        { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Function      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  deriving (Eq, Ord, Show, Generic, Hashable)

packageVertex :: PackageInfo -> Vertex
packageVertex (PackageInfo name _) = Package (formatName name)

moduleVertex :: ModuleInfo -> Vertex
moduleVertex = Module . T.pack . modulePath

unknownModuleVertex :: ModuleInfo -> Vertex
unknownModuleVertex = UnknownModule . T.pack . modulePath

variableVertex :: Text -> ModuleInfo -> Span -> Vertex
variableVertex name ModuleInfo{..} = Variable name (T.pack modulePath)

methodVertex :: Text -> ModuleInfo -> Span -> Vertex
methodVertex name ModuleInfo{..} = Method name (T.pack modulePath)

functionVertex :: Text -> ModuleInfo -> Span -> Vertex
functionVertex name ModuleInfo{..} = Function name (T.pack modulePath)

instance ToJSON Vertex where
  toJSON v = object [ "name" .= vertexIdentifier v, "type" .= vertexToType v ]

vertexIdentifier :: Vertex -> Text
vertexIdentifier v@Package{..}  = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v@Module{..}   = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v@UnknownModule{..}   = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v = vertexModuleName v <> "::" <> vertexName v <> " (" <> vertexToType v <> " " <> showSpan (vertexSpan v) <>  ")"

showSpan :: Span -> Text
showSpan (Span (Pos a b) (Pos c d)) = T.pack $
     "[" <> show a <> ", " <> show b <> "]"
  <> " - "
  <> "[" <> show c <> ", " <> show d <> "]"

vertexToType :: Vertex -> Text
vertexToType Package{}       = "Package"
vertexToType Module{}        = "Module"
vertexToType UnknownModule{} = "Unknown Module"
vertexToType Variable{}      = "Variable"
vertexToType Method{}        = "Method"
vertexToType Function{}      = "Function"

instance Lower Vertex where
  lowerBound = Package ""

class VertexDeclaration syntax where
  toVertex :: (Declarations1 syntax, Foldable syntax, HasField fields Span)
           => Record fields
           -> ModuleInfo
           -> syntax (Term syntax (Record fields))
           -> Maybe (Vertex, Name)

instance (VertexDeclaration' syntax syntax) => VertexDeclaration syntax where
  toVertex = toVertex'

class VertexDeclaration' whole syntax where
  toVertex' :: (Declarations1 whole, Foldable whole, HasField fields Span)
            => Record fields
            -> ModuleInfo
            -> syntax (Term whole (Record fields))
            -> Maybe (Vertex, Name)

instance (VertexDeclarationStrategy syntax ~ strategy, VertexDeclarationWithStrategy strategy whole syntax) => VertexDeclaration' whole syntax where
  toVertex' = toVertexWithStrategy (Proxy :: Proxy strategy)

data Strategy = Default | Custom

type family VertexDeclarationStrategy syntax where
  VertexDeclarationStrategy Syntax.Identifier = 'Custom
  VertexDeclarationStrategy Declaration.Function = 'Custom
  VertexDeclarationStrategy Declaration.Method = 'Custom
  VertexDeclarationStrategy Expression.MemberAccess = 'Custom
  VertexDeclarationStrategy (Sum _) = 'Custom
  VertexDeclarationStrategy syntax  = 'Default

class VertexDeclarationWithStrategy (strategy :: Strategy) whole syntax where
  toVertexWithStrategy :: (Declarations1 whole, Foldable whole, HasField fields Span)
                       => proxy strategy
                       -> Record fields
                       -> ModuleInfo
                       -> syntax (Term whole (Record fields))
                       -> Maybe (Vertex, Name)

-- | The 'Default' strategy produces 'Nothing'.
instance VertexDeclarationWithStrategy 'Default whole syntax where
  toVertexWithStrategy _ _ _ _ = Nothing

instance Apply (VertexDeclaration' whole) fs => VertexDeclarationWithStrategy 'Custom whole (Sum fs) where
  toVertexWithStrategy _ ann info = apply @(VertexDeclaration' whole) (toVertex' ann info)

instance VertexDeclarationWithStrategy 'Custom whole Syntax.Identifier where
  toVertexWithStrategy _ ann info (Syntax.Identifier name) = Just (variableVertex (formatName name) info (getField ann), name)

instance VertexDeclarationWithStrategy 'Custom whole Declaration.Function where
  toVertexWithStrategy _ ann info term@Declaration.Function{} = (\n -> (functionVertex (formatName n) info (getField ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy 'Custom whole Declaration.Method where
  toVertexWithStrategy _ ann info term@Declaration.Method{} = (\n -> (methodVertex (formatName n) info (getField ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy 'Custom whole whole => VertexDeclarationWithStrategy 'Custom whole Expression.MemberAccess where
  toVertexWithStrategy proxy ann info (Expression.MemberAccess (Term (In lhsAnn lhs)) name) =
    case toVertexWithStrategy proxy lhsAnn info lhs of
      Just (Variable n _ _, _) -> Just (variableVertex (n <> "." <> formatName name) info (getField ann), name)
      _ -> Just (variableVertex (formatName name) info (getField ann), name)
