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
import           Data.Record
import           Data.Span
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import           Data.Term
import qualified Data.Text as T
import           GHC.Exts (fromList)
import           Prologue hiding (packageName)
import           Proto3.Suite
import qualified Proto3.Suite as PB
import qualified Proto3.Wire.Encode as Encode

-- | A vertex of representing some node in a control flow graph.
data ControlFlowVertex
  = Package       { vertexName :: Text }
  | Module        { vertexName :: Text }
  | UnknownModule { vertexName :: Text }
  | Variable      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Method        { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  | Function      { vertexName :: Text, vertexModuleName :: Text, vertexSpan :: Span }
  deriving (Eq, Ord, Show, Generic, Hashable, Named)

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

instance Named (G.Graph ControlFlowVertex) where nameOf _ = "ControlFlowGraph"

instance Message (G.Graph ControlFlowVertex) where
  encodeMessage _ graph =  encodeMessageField 1 (NestedVec (fromList (G.vertexList graph)))
                        <> encodeMessageField 2 (NestedVec (fromList (G.edgeList graph)))
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Repeated . Named $ Single "ControlFlowVertex") (Single "vertices") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Repeated . Named $ Single "ControlFlowEdge")  (Single "edges") [] Nothing
    ]

instance Lower ControlFlowVertex where lowerBound = Package ""
instance VertexTag ControlFlowVertex where uniqueTag = hash . vertexIdentifier

instance ToJSON ControlFlowVertex where
  toJSON v = object [ "name" .= vertexIdentifier v, "type" .= vertexToType v ]

instance Message ControlFlowVertex where
  encodeMessage _ v@Package{..}       = Encode.embedded 1 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v))
  encodeMessage _ v@Module{..}        = Encode.embedded 2 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v))
  encodeMessage _ v@UnknownModule{..} = Encode.embedded 3 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v))
  encodeMessage _ v@Variable{..}      = Encode.embedded 4 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v) <> encodePrimitive 4 vertexModuleName <> Encode.embedded 5 (encodeMessage 1 vertexSpan))
  encodeMessage _ v@Method{..}        = Encode.embedded 5 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v) <> encodePrimitive 4 vertexModuleName <> Encode.embedded 5 (encodeMessage 1 vertexSpan))
  encodeMessage _ v@Function{..}      = Encode.embedded 6 (encodePrimitive 1 (uniqueTag v) <> encodePrimitive 2 vertexName <> encodePrimitive 3 (vertexIdentifier v) <> encodePrimitive 4 vertexModuleName <> Encode.embedded 5 (encodeMessage 1 vertexSpan))
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageOneOf (Single "vertex")
      [ DotProtoField 1 (Prim . Named $ Single "Package") (Single "package") [] Nothing
      , DotProtoField 2 (Prim . Named $ Single "Module") (Single "module") [] Nothing
      , DotProtoField 3 (Prim . Named $ Single "UnknownModule") (Single "unknownModule") [] Nothing
      , DotProtoField 4 (Prim . Named $ Single "Variable") (Single "variable") [] Nothing
      , DotProtoField 5 (Prim . Named $ Single "Method") (Single "method") [] Nothing
      , DotProtoField 6 (Prim . Named $ Single "Function") (Single "function") [] Nothing
      ]
    ]
    <> gen "Package" mempty
    <> gen "Module" mempty
    <> gen "UnknownModule" mempty
    <> gen "Variable" [ genModuleName, genSpan ]
    <> gen "Method" [ genModuleName, genSpan ]
    <> gen "Function" [ genModuleName, genSpan ]
    where
      genModuleName = DotProtoMessageField $ DotProtoField 4 (Prim PB.String) (Single "moduleName") [] Nothing
      genSpan = DotProtoMessageField $ DotProtoField 5 (Prim . Named $ Single (nameOf (Proxy @Span))) (Single "span") [] Nothing
      gen name extras =
        [ DotProtoMessageDefinition . DotProtoMessage (Single name) $
            (DotProtoMessageField $ DotProtoField 1 (Prim PB.Int64) (Single "id") [] Nothing)
            : (DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "name") [] Nothing)
            : (DotProtoMessageField $ DotProtoField 3 (Prim PB.String) (Single "description") [] Nothing)
            : extras
        ]


instance Named (G.Edge ControlFlowVertex) where nameOf _ = "ControlFlowEdge"

instance Message (G.Edge ControlFlowVertex) where
  encodeMessage _ (G.Edge (from, to)) = encodePrimitive 1 (uniqueTag from) <> encodePrimitive 2 (uniqueTag to)
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.Int64) (Single "from") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.Int64) (Single "to")   [] Nothing
    ]


-- TODO: This is potentially valuable just to get name's out of declarable things.
-- Typeclasses to create 'ControlFlowVertex's from 'Term's. Also extracts
-- 'Name's for terms with symbolic names like Identifiers and Declarations.

class VertexDeclaration syntax where
  toVertex :: (Declarations1 syntax, Foldable syntax, HasField fields Span)
           => Record fields
           -> ModuleInfo
           -> syntax (Term syntax (Record fields))
           -> Maybe (ControlFlowVertex, Name)

instance (VertexDeclaration' syntax syntax) => VertexDeclaration syntax where
  toVertex = toVertex'

class VertexDeclaration' whole syntax where
  toVertex' :: (Declarations1 whole, Foldable whole, HasField fields Span)
            => Record fields
            -> ModuleInfo
            -> syntax (Term whole (Record fields))
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
  toVertexWithStrategy :: (Declarations1 whole, Foldable whole, HasField fields Span)
                       => proxy strategy
                       -> Record fields
                       -> ModuleInfo
                       -> syntax (Term whole (Record fields))
                       -> Maybe (ControlFlowVertex, Name)

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
