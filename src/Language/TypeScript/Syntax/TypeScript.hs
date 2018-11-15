{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DuplicateRecordFields, TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.TypeScript where

import Prologue

import           Data.Aeson
import qualified Data.Text as T
import           Proto3.Suite

import Control.Abstract hiding (Import)
import           Data.Abstract.Evaluatable as Evaluatable
import           qualified Data.Abstract.ScopeGraph as ScopeGraph
import           qualified Data.Abstract.Heap as Heap
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Language.TypeScript.Resolution
import qualified Data.Map.Strict as Map

data Import a = Import { importSymbols :: ![Alias], importFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

  -- http://www.typescriptlang.org/docs/handbook/module-resolution.html
instance Evaluatable Import where
  eval _ (Import symbols importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    (scopeGraph, (heap, _)) <- require modulePath
    bindAll scopeGraph
    bindFrames heap
    if Prologue.null symbols then
      maybe (pure ()) insertImportEdge (ScopeGraph.currentScope scopeGraph)
    else do
      let scopeEdges = maybe mempty (Map.singleton ScopeGraph.Import . pure) (ScopeGraph.currentScope scopeGraph)
      scopeAddress <- newScope scopeEdges
      scope <- lookupScope scopeAddress
      for_ symbols $ \Alias{..} ->
        insertImportReference (Reference aliasName) (Declaration aliasValue) scopeGraph scopeAddress scope
      let frameLinks = case (ScopeGraph.currentScope scopeGraph, Heap.currentFrame heap) of
            (Just importScope, Just importFrame) -> Map.singleton importScope importFrame
            _ -> mempty

      frameAddress <- newFrame scopeAddress (Map.singleton ScopeGraph.Import frameLinks)
      insertImportEdge scopeAddress
      insertFrameLink ScopeGraph.Import (Map.singleton scopeAddress frameAddress)
    rvalBox unit

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportAlias :: !a, qualifiedAliasedImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedAliasedImport where liftEq = genericLiftEq
instance Ord1 QualifiedAliasedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedAliasedImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedAliasedImport where
  eval _ (QualifiedAliasedImport aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    -- alias <- maybeM (throwEvalError NoNameError) (declaredName aliasTerm)
    -- rvalBox =<< evalRequire modulePath alias
    alias <- maybeM (throwEvalError NoNameError) (declaredName aliasTerm)
    span <- get @Span
    (scopeGraph, (_, _)) <- require modulePath
    bindAll scopeGraph
    declare (Declaration alias) span (ScopeGraph.currentScope scopeGraph)
    rvalBox unit

newtype SideEffectImport a = SideEffectImport { sideEffectImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 SideEffectImport where liftEq = genericLiftEq
instance Ord1 SideEffectImport where liftCompare = genericLiftCompare
instance Show1 SideEffectImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SideEffectImport where
  eval _ (SideEffectImport importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    (scopeGraph, _) <- require modulePath
    bindAll scopeGraph
    rvalBox unit


-- | Qualified Export declarations
newtype QualifiedExport a = QualifiedExport { qualifiedExportSymbols :: [Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExport where liftEq = genericLiftEq
instance Ord1 QualifiedExport where liftCompare = genericLiftCompare
instance Show1 QualifiedExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExport where
  -- eval _ (QualifiedExport exportSymbols) = do
  --   -- Insert the aliases with no addresses.
  --   for_ exportSymbols $ \Alias{..} ->
  --     export aliasValue aliasName Nothing
  eval _ (QualifiedExport exportSymbols) = do
    -- Create a Lexical edge from the qualifed export's scope to the current scope.
    currentScopeAddress <- currentScope
    let edges = maybe mempty (Map.singleton Lexical . pure) currentScopeAddress
    exportScope <- newScope edges
    insertExportEdge exportScope -- Create an export edge from the current scope to the export scope
    putCurrentScope exportScope -- Set the export scope as the new current scope

    for_ exportSymbols $ \Alias{..} -> do
      reference (Reference aliasName) (Declaration aliasValue)

    -- Create an export edge from a new scope to the qualifed export's scope.
    rvalBox unit

data Alias = Alias { aliasValue :: Name, aliasName :: Name }
  deriving (Eq, Generic, Hashable, Ord, Show, Message, Named, ToJSON, NFData)

toTuple :: Alias -> (Name, Name)
toTuple Alias{..} = (aliasValue, aliasName)

-- | Qualified Export declarations that export from another module.
data QualifiedExportFrom a = QualifiedExportFrom { qualifiedExportFrom :: ImportPath, qualifiedExportFromSymbols :: ![Alias]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExportFrom where liftEq = genericLiftEq
instance Ord1 QualifiedExportFrom where liftCompare = genericLiftCompare
instance Show1 QualifiedExportFrom where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExportFrom where
  eval _ (QualifiedExportFrom importPath exportSymbols) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    -- scopeGraph <- fst <$> require modulePath
    -- Look up addresses in importedEnv and insert the aliases with addresses into the exports.
    for_ exportSymbols $ \Alias{..} -> do
      -- TODO: Add an Alias Edge to resolve qualified export froms
      -- Scope 1 -> alias (bar, foo) -> Export 3 -> Export -> Scope 4
      pure ()
      -- let address = Env.lookup aliasValue importedBinds
      -- maybe (throwEvalError $ ExportError modulePath aliasValue) (export aliasValue aliasName . Just) address
    rvalBox unit

newtype DefaultExport a = DefaultExport { defaultExport :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultExport where liftEq = genericLiftEq
instance Ord1 DefaultExport where liftCompare = genericLiftCompare
instance Show1 DefaultExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultExport where
  eval _ (DefaultExport term) = do
    case declaredName term of
      Just _ -> undefined -- do
        -- addr <- eval term >>= address
        -- export name name Nothing
        -- bind name addr
      Nothing -> throwEvalError DefaultExportError
    rvalBox unit


-- | Lookup type for a type-level key in a typescript map.
data LookupType a = LookupType { lookupTypeIdentifier :: a, lookupTypeKey :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LookupType where liftEq = genericLiftEq
instance Ord1 LookupType where liftCompare = genericLiftCompare
instance Show1 LookupType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LookupType

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Ord1 ShorthandPropertyIdentifier where liftCompare = genericLiftCompare
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ShorthandPropertyIdentifier

data Union a = Union { unionLeft :: !a, unionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Language.TypeScript.Syntax.TypeScript.Union where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.TypeScript.Union where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.TypeScript.Union where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.TypeScript.Union

data Intersection a = Intersection { intersectionLeft :: !a, intersectionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Ord1 Intersection where liftCompare = genericLiftCompare
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Intersection

data FunctionType a = FunctionType { functionTypeParameters :: !a, functionFormalParameters :: ![a], functionType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FunctionType

data AmbientFunction a = AmbientFunction { ambientFunctionContext :: ![a], ambientFunctionIdentifier :: !a, ambientFunctionParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Ord1 AmbientFunction where liftCompare = genericLiftCompare
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AmbientFunction

data ImportRequireClause a = ImportRequireClause { importRequireIdentifier :: !a, importRequireSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportRequireClause where liftEq = genericLiftEq
instance Ord1 ImportRequireClause where liftCompare = genericLiftCompare
instance Show1 ImportRequireClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportRequireClause

newtype ImportClause a = ImportClause { importClauseElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportClause where liftEq = genericLiftEq
instance Ord1 ImportClause where liftCompare = genericLiftCompare
instance Show1 ImportClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportClause

newtype Tuple a = Tuple { tupleElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- This is a tuple type, not a tuple value, so we can't lean on the shared Tuple value
instance Evaluatable Tuple

data Constructor a = Constructor { constructorTypeParameters :: !a, constructorFormalParameters :: ![a], constructorType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Language.TypeScript.Syntax.TypeScript.Constructor where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.TypeScript.Constructor where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.TypeScript.Constructor where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.TypeScript.Constructor

data TypeParameter a = TypeParameter { typeParameter :: !a, typeParameterConstraint :: !a, typeParameterDefaultType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Ord1 TypeParameter where liftCompare = genericLiftCompare
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeParameter

data TypeAssertion a = TypeAssertion { typeAssertionParameters :: !a, typeAssertionExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeAssertion where liftEq = genericLiftEq
instance Ord1 TypeAssertion where liftCompare = genericLiftCompare
instance Show1 TypeAssertion where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeAssertion

newtype Annotation a = Annotation { annotationType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Annotation

newtype Decorator a = Decorator { decoratorTerm :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Decorator

newtype ComputedPropertyName a = ComputedPropertyName { propertyName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ComputedPropertyName where liftEq = genericLiftEq
instance Ord1 ComputedPropertyName where liftCompare = genericLiftCompare
instance Show1 ComputedPropertyName where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ComputedPropertyName

newtype Constraint a = Constraint { constraintType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Ord1 Constraint where liftCompare = genericLiftCompare
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Constraint

newtype DefaultType a = DefaultType { defaultType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultType where liftEq = genericLiftEq
instance Ord1 DefaultType where liftCompare = genericLiftCompare
instance Show1 DefaultType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable DefaultType

newtype ParenthesizedType a = ParenthesizedType { parenthesizedType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Ord1 ParenthesizedType where liftCompare = genericLiftCompare
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ParenthesizedType

newtype PredefinedType a = PredefinedType { predefinedType :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Ord1 PredefinedType where liftCompare = genericLiftCompare
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec
-- TODO: Implement Eval instance for PredefinedType
instance Evaluatable PredefinedType

newtype TypeIdentifier a = TypeIdentifier { contents :: T.Text }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Declarations1 TypeIdentifier where
  liftDeclaredName _ (TypeIdentifier identifier) = Just (Evaluatable.name identifier)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Ord1 TypeIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec
-- TODO: TypeIdentifier shouldn't evaluate to an address in the heap?
instance Evaluatable TypeIdentifier where
  eval _ TypeIdentifier{..} = do
    -- Add a reference to the type identifier in the current scope.
    reference (Reference (Evaluatable.name contents)) (Declaration (Evaluatable.name contents))
    rvalBox unit

data NestedIdentifier a = NestedIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Ord1 NestedIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedIdentifier

data NestedTypeIdentifier a = NestedTypeIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NestedTypeIdentifier where liftEq = genericLiftEq
instance Ord1 NestedTypeIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedTypeIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedTypeIdentifier

data GenericType a = GenericType { genericTypeIdentifier :: !a, genericTypeArguments :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Ord1 GenericType where liftCompare = genericLiftCompare
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable GenericType

data TypePredicate a = TypePredicate { typePredicateIdentifier :: !a, typePredicateType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypePredicate where liftEq = genericLiftEq
instance Ord1 TypePredicate where liftCompare = genericLiftCompare
instance Show1 TypePredicate where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypePredicate

newtype ObjectType a = ObjectType { objectTypeElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ObjectType where liftEq = genericLiftEq
instance Ord1 ObjectType where liftCompare = genericLiftCompare
instance Show1 ObjectType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ObjectType

newtype AmbientDeclaration a = AmbientDeclaration { ambientDeclarationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Ord1 AmbientDeclaration where liftCompare = genericLiftCompare
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AmbientDeclaration where
  eval eval (AmbientDeclaration body) = eval body

data EnumDeclaration a = EnumDeclaration { enumDeclarationIdentifier :: !a, enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable EnumDeclaration

instance Declarations a => Declarations (EnumDeclaration a) where
  declaredName EnumDeclaration{..} = declaredName enumDeclarationIdentifier

newtype ExtendsClause a = ExtendsClause { extendsClauses :: [a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Declarations1 ExtendsClause where
  liftDeclaredName _ (ExtendsClause []) = Nothing
  liftDeclaredName declaredName (ExtendsClause (x : _)) = declaredName x

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Ord1 ExtendsClause where liftCompare = genericLiftCompare
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec
-- TODO: ExtendsClause shouldn't evaluate to an address in the heap?
instance Evaluatable ExtendsClause where
  eval eval ExtendsClause{..} = do
    -- Evaluate subterms
    traverse_ eval extendsClauses
    rvalBox unit

newtype ArrayType a = ArrayType { arrayType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ArrayType

newtype FlowMaybeType a = FlowMaybeType { flowMaybeType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FlowMaybeType where liftEq = genericLiftEq
instance Ord1 FlowMaybeType where liftCompare = genericLiftCompare
instance Show1 FlowMaybeType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FlowMaybeType

newtype TypeQuery a = TypeQuery { typeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeQuery where liftEq = genericLiftEq
instance Ord1 TypeQuery where liftCompare = genericLiftCompare
instance Show1 TypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeQuery

newtype IndexTypeQuery a = IndexTypeQuery { indexTypeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 IndexTypeQuery where liftEq = genericLiftEq
instance Ord1 IndexTypeQuery where liftCompare = genericLiftCompare
instance Show1 IndexTypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexTypeQuery

newtype TypeArguments a = TypeArguments { typeArguments :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeArguments where liftEq = genericLiftEq
instance Ord1 TypeArguments where liftCompare = genericLiftCompare
instance Show1 TypeArguments where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeArguments

newtype ThisType a = ThisType { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ThisType where liftEq = genericLiftEq
instance Ord1 ThisType where liftCompare = genericLiftCompare
instance Show1 ThisType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ThisType

newtype ExistentialType a = ExistentialType { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ExistentialType where liftEq = genericLiftEq
instance Ord1 ExistentialType where liftCompare = genericLiftCompare
instance Show1 ExistentialType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExistentialType

newtype LiteralType a = LiteralType { literalTypeSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LiteralType where liftEq = genericLiftEq
instance Ord1 LiteralType where liftCompare = genericLiftCompare
instance Show1 LiteralType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LiteralType

data PropertySignature a = PropertySignature { modifiers :: ![a], propertySignaturePropertyName :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Ord1 PropertySignature where liftCompare = genericLiftCompare
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable PropertySignature

data CallSignature a = CallSignature { callSignatureTypeParameters :: !a, callSignatureParameters :: ![a], callSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Ord1 CallSignature where liftCompare = genericLiftCompare
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable CallSignature

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { constructSignatureTypeParameters :: !a, constructSignatureParameters :: ![a], constructSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Ord1 ConstructSignature where liftCompare = genericLiftCompare
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ConstructSignature

data IndexSignature a = IndexSignature { indexSignatureSubject :: a, indexSignatureType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Ord1 IndexSignature where liftCompare = genericLiftCompare
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexSignature

data AbstractMethodSignature a = AbstractMethodSignature { abstractMethodSignatureContext :: ![a], abstractMethodSignatureName :: !a, abstractMethodSignatureParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AbstractMethodSignature where liftEq = genericLiftEq
instance Ord1 AbstractMethodSignature where liftCompare = genericLiftCompare
instance Show1 AbstractMethodSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AbstractMethodSignature

data ForOf a = ForOf { forOfBinding :: !a, forOfSubject :: !a, forOfBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Ord1 ForOf where liftCompare = genericLiftCompare
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ForOf

data LabeledStatement a = LabeledStatement { labeledStatementIdentifier :: !a, labeledStatementSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LabeledStatement

newtype Update a = Update { updateSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Update

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval _ (Module _ _) = undefined -- do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName iden)
    -- rvalBox =<< letrec' name (\addr ->
    --   makeNamespace name addr Nothing (traverse_ eval xs))

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier

data InternalModule a = InternalModule { internalModuleIdentifier :: !a, internalModuleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Ord1 InternalModule where liftCompare = genericLiftCompare
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InternalModule where
  eval _ (InternalModule _ _) = undefined -- do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName iden)
    -- rvalBox =<< letrec' name (\addr ->
    --   makeNamespace name addr Nothing (traverse_ eval xs))

instance Declarations a => Declarations (InternalModule a) where
  declaredName InternalModule{..} = declaredName internalModuleIdentifier


data ImportAlias a = ImportAlias { importAliasSubject :: !a, importAlias :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportAlias

data ClassHeritage a = ClassHeritage { classHeritageExtendsClause :: !a, implementsClause :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Ord1 ClassHeritage where liftCompare = genericLiftCompare
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ClassHeritage

data AbstractClass a = AbstractClass { abstractClassIdentifier :: !a,  abstractClassTypeParameters :: !a, classHeritage :: ![a], classBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Ord1 AbstractClass where liftCompare = genericLiftCompare
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec
instance Declarations a => Declarations (AbstractClass a) where
  declaredName AbstractClass{..} = declaredName abstractClassIdentifier

instance Evaluatable AbstractClass where
  eval _ AbstractClass{..} = undefined -- do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName abstractClassIdentifier)
    -- supers <- traverse (eval >=> address) classHeritage
    -- (v, addr) <- letrec name $ do
    --   void $ eval classBody
    --   classBinds <- Env.head <$> getEnv
    --   klass name supers classBinds
    -- rvalBox =<< (v <$ bind name addr)

  -- Previous ScopeGraph approach:
  -- eval AbstractClass{..} = do
  --   name <- maybeM (throwEvalError NoNameError) (declaredName (subterm abstractClassIdentifier))
  --   span <- ask @Span
  --   -- Run the action within the class's scope.
  --   currentScopeAddress <- currentScope
  --
  --   supers <- for classHeritage $ \superclass -> do
  --     name <- maybeM (throwEvalError NoNameError) (declaredName (subterm superclass))
  --     scope <- associatedScope (Declaration name)
  --     (scope,) <$> subtermValue superclass
  --
  --   let imports = (ScopeGraph.Import, ) <$> (pure . catMaybes $ fst <$> supers)
  --       current = pure (Lexical, [ currentScopeAddress ])
  --       edges = Map.fromList (imports <> current)
  --   childScope <- newScope edges
  --   declare (Declaration name) span (Just childScope)
  --
  --   frame <- newFrame childScope mempty -- TODO: Instantiate frames for superclasses
  --   withScopeAndFrame frame $ do
  --     void $ subtermValue classBody
  --     klass (Declaration name) (snd <$> supers) frame
  --
  --   rvalBox unit
