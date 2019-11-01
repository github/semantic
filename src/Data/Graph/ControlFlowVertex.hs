{-# LANGUAGE DataKinds, EmptyCase, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
, toVertex1
, VertexDeclaration1 (..)
) where

import           Data.Abstract.Declarations
import           Data.Abstract.Module (ModuleInfo (..))
import           Data.Abstract.Name
import           Data.Abstract.Package (PackageInfo (..))
import           Data.Aeson
import           Data.Graph (VertexTag (..))
import qualified Data.Graph as G
import           Data.Quieterm (Quieterm(..))
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import           Data.Term
import qualified Data.Text as T
import           GHC.Generics (V1)
import           Prelude hiding (span)
import           Prologue
import qualified Source.Loc as Loc
import           Source.Span

-- | A vertex of representing some node in a control flow graph.
data ControlFlowVertex
  = Package       Text
  | Module        Text
  | UnknownModule Text
  | Variable      Text Text Span
  | Method        Text Text Span
  | Function      Text Text Span
  deriving (Eq, Ord, Show)

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
vertexIdentifier v = case v of
  Package       n     -> n <> " (" <> vertexToType v <> ")"
  Module        n     -> n <> " (" <> vertexToType v <> ")"
  UnknownModule n     -> n <> " (" <> vertexToType v <> ")"
  Variable      n m s -> m <> "::" <> n <> " (" <> vertexToType v <> " " <> showSpan s <>  ")"
  Method        n m s -> m <> "::" <> n <> " (" <> vertexToType v <> " " <> showSpan s <>  ")"
  Function      n m s -> m <> "::" <> n <> " (" <> vertexToType v <> " " <> showSpan s <>  ")"

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
  toJSON v = object [ "name" .= vertexIdentifier v
                    , "type" .= vertexToType v
                    , "id"   .= show (uniqueTag v)
                    ]
  toEncoding v = pairs $ mconcat [ "name" .= vertexIdentifier v
                                 , "type" .= vertexToType v
                                 , "id"   .= show (uniqueTag v)
                                 ]

-- TODO: This is potentially valuable just to get name's out of declarable things.
-- Typeclasses to create 'ControlFlowVertex's from 'Term's. Also extracts
-- 'Name's for terms with symbolic names like Identifiers and Declarations.

class VertexDeclaration term where
  toVertex
    :: ModuleInfo
    -> term Loc.Loc
    -> Maybe (ControlFlowVertex, Name)

instance (VertexDeclaration1 f, Declarations1 f) => VertexDeclaration (Term f) where
  toVertex info (Term (In a f)) = liftToVertex toVertex a info f

instance (VertexDeclaration1 f, Declarations1 f) => VertexDeclaration (Quieterm f) where
  toVertex info (Quieterm (In a f)) = liftToVertex toVertex a info f

toVertex1 :: (VertexDeclaration1 f, VertexDeclaration t, Declarations (t Loc.Loc)) => Loc.Loc -> ModuleInfo -> f (t Loc.Loc) -> Maybe (ControlFlowVertex, Name)
toVertex1 = liftToVertex toVertex

class VertexDeclaration1 syntax where
  liftToVertex :: Declarations (term Loc.Loc)
               => (ModuleInfo -> term Loc.Loc -> Maybe (ControlFlowVertex, Name))
               -> Loc.Loc
               -> ModuleInfo
               -> syntax (term Loc.Loc)
               -> Maybe (ControlFlowVertex, Name)

instance (VertexDeclarationStrategy1 syntax ~ strategy, VertexDeclarationWithStrategy1 strategy syntax) => VertexDeclaration1 syntax where
  liftToVertex = liftToVertexWithStrategy (Proxy :: Proxy strategy)

-- | This appears to be required to convince 'Semantic.Graph.runCallGraph' not to try to specialize the instance too eagerly.
instance {-# OVERLAPPING #-} VertexDeclaration1 V1 where
  liftToVertex _ _ _ v = case v of {}

data Strategy = Default | Custom

type family VertexDeclarationStrategy1 syntax where
  VertexDeclarationStrategy1 Syntax.Identifier       = 'Custom
  VertexDeclarationStrategy1 Declaration.Function    = 'Custom
  VertexDeclarationStrategy1 Declaration.Method      = 'Custom
  VertexDeclarationStrategy1 Expression.MemberAccess = 'Custom
  VertexDeclarationStrategy1 (Sum _)                 = 'Custom
  VertexDeclarationStrategy1 _                       = 'Default

class VertexDeclarationWithStrategy1 (strategy :: Strategy) syntax where
  liftToVertexWithStrategy :: Declarations (term Loc.Loc)
                           => proxy strategy
                           -> (ModuleInfo -> term Loc.Loc -> Maybe (ControlFlowVertex, Name))
                           -> Loc.Loc
                           -> ModuleInfo
                           -> syntax (term Loc.Loc)
                           -> Maybe (ControlFlowVertex, Name)

-- | The 'Default' strategy produces 'Nothing'.
instance VertexDeclarationWithStrategy1 'Default syntax where
  liftToVertexWithStrategy _ _ _ _ _ = Nothing

instance Apply VertexDeclaration1 fs => VertexDeclarationWithStrategy1 'Custom (Sum fs) where
  liftToVertexWithStrategy _ toVertex ann info = apply @VertexDeclaration1 (liftToVertex toVertex ann info)

instance VertexDeclarationWithStrategy1 'Custom Syntax.Identifier where
  liftToVertexWithStrategy _ _ ann info (Syntax.Identifier name) = Just (variableVertex (formatName name) info (Loc.span ann), name)

instance VertexDeclarationWithStrategy1 'Custom Declaration.Function where
  liftToVertexWithStrategy _ _ ann info term@Declaration.Function{} = (\n -> (functionVertex (formatName n) info (Loc.span ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy1 'Custom Declaration.Method where
  liftToVertexWithStrategy _ _ ann info term@Declaration.Method{} = (\n -> (methodVertex (formatName n) info (Loc.span ann), n)) <$> liftDeclaredName declaredName term

instance VertexDeclarationWithStrategy1 'Custom Expression.MemberAccess where
  liftToVertexWithStrategy _ toVertex ann info (Expression.MemberAccess lhs rhs) =
    case (toVertex info lhs, toVertex info rhs) of
      (Just (Variable n _ _, _), Just (_, name)) -> Just (variableVertex (n <> "." <> formatName name) info (Loc.span ann), name)
      (_, Just (_, name)) -> Just (variableVertex (formatName name) info (Loc.span ann), name)
      _ -> Nothing
