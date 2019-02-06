{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Graph.ControlFlowVertex
( ControlFlowVertex (..)
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
import           Data.Graph (VertexTag (..))
import qualified Data.Graph as G
import           Data.Location
import           Data.Span
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import           Data.Term
import qualified Data.Text as T
import           Prologue

-- | A vertex of representing some node in a control flow graph.
data ControlFlowVertex
  = Package       { vertexName :: Text }
  | Module        { vertexName :: Text }
  | UnknownModule { vertexName :: Text }
  | Variable      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Method        { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Function      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

packageVertex :: PackageInfo -> ControlFlowVertex
packageVertex (PackageInfo name _) = Package (formatName name)

moduleVertex :: ModuleInfo -> ControlFlowVertex
moduleVertex = Module . T.pack . modulePath

unknownModuleVertex :: ModuleInfo -> ControlFlowVertex
unknownModuleVertex = UnknownModule . T.pack . modulePath

variableVertex :: Text -> ModuleInfo -> Span -> ControlFlowVertex
variableVertex name ModuleInfo{..} = Variable name (T.pack modulePath)

methodVertex :: Text -> ModuleInfo -> Span -> ControlFlowVertex
methodVertex name ModuleInfo{..} = Method name (T.pack modulePath)

functionVertex :: Text -> ModuleInfo -> Span -> ControlFlowVertex
functionVertex name ModuleInfo{..} = Function name (T.pack modulePath)

vertexIdentifier :: ControlFlowVertex -> Text
vertexIdentifier v@Package{..}  = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v@Module{..}   = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v@UnknownModule{..}   = vertexName <> " (" <> vertexToType v <> ")"
vertexIdentifier v = vertexModuleName v <> "::" <> vertexName v <> " (" <> vertexToType v <> " " <> showSpan (vertexSpan v) <>  ")"

showSpan :: Span -> Text
showSpan (Span (Pos a b) (Pos c d)) = T.pack $
     "[" <> show a <> ", " <> show b <> "]"
  <> " - "
  <> "[" <> show c <> ", " <> show d <> "]"

vertexToType :: ControlFlowVertex -> Text
vertexToType Package{}       = "Package"
vertexToType Module{}        = "Module"
vertexToType UnknownModule{} = "Unknown Module"
vertexToType Variable{}      = "Variable"
vertexToType Method{}        = "Method"
vertexToType Function{}      = "Function"


-- Instances

instance Lower ControlFlowVertex where lowerBound = Package ""
instance VertexTag ControlFlowVertex where uniqueTag = hash . vertexIdentifier

instance ToJSON ControlFlowVertex where
  toJSON v = object [ "name" .= vertexIdentifier v, "type" .= vertexToType v ]

-- TODO: This is potentially valuable just to get name's out of declarable things.
-- Typeclasses to create 'ControlFlowVertex's from 'Term's. Also extracts
-- 'Name's for terms with symbolic names like Identifiers and Declarations.

class VertexDeclaration syntax where
  toVertex :: (Declarations1 syntax, Foldable syntax)
           => Location
           -> ModuleInfo
           -> syntax (Term syntax Location)
           -> Maybe (ControlFlowVertex, Name)

instance (VertexDeclaration' syntax syntax) => VertexDeclaration syntax where
  toVertex = toVertex'

class VertexDeclaration' whole syntax where
  toVertex' :: (Declarations1 whole, Foldable whole)
            => Location
            -> ModuleInfo
            -> syntax (Term whole Location)
            -> Maybe (ControlFlowVertex, Name)

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
  toVertexWithStrategy :: (Declarations1 whole, Foldable whole)
                       => proxy strategy
                       -> Location
                       -> ModuleInfo
                       -> syntax (Term whole Location)
                       -> Maybe (ControlFlowVertex, Name)

-- | The 'Default' strategy produces 'Nothing'.
instance VertexDeclarationWithStrategy 'Default whole syntax where
  toVertexWithStrategy _ _ _ _ = Nothing

instance Apply (VertexDeclaration' whole) fs => VertexDeclarationWithStrategy 'Custom whole (Sum fs) where
  toVertexWithStrategy _ ann info = apply @(VertexDeclaration' whole) (toVertex' ann info)

instance VertexDeclarationWithStrategy 'Custom whole Syntax.Identifier where
  toVertexWithStrategy _ ann info (Syntax.Identifier name) = Just (variableVertex (formatName name) info (locationSpan ann), name)

instance VertexDeclarationWithStrategy 'Custom whole Declaration.Function where
  toVertexWithStrategy _ ann info term@Declaration.Function{} = (\n -> (functionVertex (formatName n) info (locationSpan ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy 'Custom whole Declaration.Method where
  toVertexWithStrategy _ ann info term@Declaration.Method{} = (\n -> (methodVertex (formatName n) info (locationSpan ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy 'Custom whole whole => VertexDeclarationWithStrategy 'Custom whole Expression.MemberAccess where
  toVertexWithStrategy proxy ann info (Expression.MemberAccess (Term (In lhsAnn lhs)) name) =
    case toVertexWithStrategy proxy lhsAnn info lhs of
      Just (Variable n _ _, _) -> Just (variableVertex (n <> "." <> formatName name) info (locationSpan ann), name)
      _ -> Just (variableVertex (formatName name) info (locationSpan ann), name)
